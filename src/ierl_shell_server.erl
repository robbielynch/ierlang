%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc The Shell Server handles the most important messages received
%%%      from IPython.
%%%
%%% @end
%%% Created : 03. Apr 2014 11:51
%%%-------------------------------------------------------------------
-module (ierl_shell_server).
-author("Robbie Lynch").

-record(reply_message, {
  uuid,
  delim          = <<"<IDS|MSG>">>,
  hmac           = <<"">>,
  header,
  parent_header,
  metadata       = <<"{}">>,
  content
}).

-export ([start/3]).

%%====================================================================
%% API functions
%%====================================================================

%%% @doc Starts the shell server
start(ShellSocket, IOPubSocket, Key) ->
  loop(ShellSocket, IOPubSocket, 1, Key).

%%====================================================================
%% Internal functions
%%====================================================================

loop(ShellSocket, IOPubSocket, ExeCount, Key) ->
  shell_listener(ShellSocket, IOPubSocket, ExeCount, [], Key),

  loop(ShellSocket, IOPubSocket, ExeCount + 1, Key).

%%% @doc Listens for messages on the Shell Socket, parses and
%%%      acts upon the message contents, then replies to IPython
shell_listener(ShellSocket, IOPubSocket, ExeCount, Bindings, Key) ->
  {Header, Content} = receive_all(ShellSocket),

  case ierl_message_parser:parse([Header], header) of
    {ok, _Username, UUID, _MessageID, "kernel_info_request", Date} ->
      send_reply(
        UUID, Date, Header, Key,
        ShellSocket, "kernel_info_reply", kernel_info_reply, {}
      );

    {ok, _Username, UUID, _MessageID, "execute_request", Date} ->
      handle_execute_request(Session, Date, Header, Content, Key);

    {ok, _Username, UUID, _MessageID, "complete_request", _Date} ->
      handle_complete_request(Session, Date, Header, Key)
  end.

receive_all(ShellSocket) ->
  {ok, _UUID}         = erlzmq:recv(ShellSocket),
  {ok, _Delim}        = erlzmq:recv(ShellSocket),
  {ok, _Hmac}         = erlzmq:recv(ShellSocket),
  {ok, Header}        = erlzmq:recv(ShellSocket),
  {ok, _ParentHeader} = erlzmq:recv(ShellSocket),
  {ok, _Metadata}     = erlzmq:recv(ShellSocket),
  {ok, Content}       = erlzmq:recv(ShellSocket),

  {Header, Content}.

handle_execute_request(UUID, Date, Header, Content, Key) ->
  % 1. SEND BUSY STATUS ON IOPUB
  % 2. PARSE EXECUTE_REQUEST CONTENT
  % 3. SEND PYIN ON IOPUB
  % 4. EVALUATE THE ERLANG CODE
  % 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
  % 6. SEND PYOUT MESSAGE ON IOPUB
  % 7. SEND IDLE STATUS MESSAGE ON IOPUB

  %%% 1. SEND BUSY STATUS ON IOPUB
  send_reply(
    UUID, Date, Header, Key,
    IOPubSocket, "status", busy, {}
  ),

  %%% 2. PARSE EXECUTE_REQUEST CONTENT
  {ok, Code, _, _, _, _, _} = ierl_message_parser:parse(Content, execute_request),

  %%% 3. SEND PYIN ON IOPUB
  ierl_message_sender:send_pyin(IOPubSocket, Result, [Session, Header, Date]),

  %%% 4. EVALUATE THE ERLANG CODE
  case ierl_code_manager:module_or_expression(Code, Bindings) of
    {ok, CompileResultList} ->
      handle_compilation_result();

    {ok, Value, NewBindings} ->
      handle_code_execution();

    {error, Exception, Reason} ->
      handle_code_exception();
  end.

handle_compilation_result(),
  CompiledResult = case CompileResultList of
    []             -> "Successfully Compiled";
    CompileMessage -> CompileMessage
  end,

  %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
  send_reply(
    UUID, Date, Header, Key,
    ShellSocket, "execute_reply", execute_reply, {"ok", ExeCount, {}, {}}
  ).

  %%% 6. SEND PYOUT MESSAGE ON IOPUB
  ierl_message_sender:send_pyout(IOPubSocket, Result, [Session, Header, Date, ExeCount]).

  %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
  send_reply(
    UUID, Date, ParentHeader, Key,
    IOPubSocket, "status", idle, {}
  ).

handle_code_execution() ->
  %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
  send_reply(
    UUID, Date, ParentHeader, Key,
    ShellSocket, "execute_reply", execute_reply, {"ok", ExeCount, {}, {}}
  ).

  %%% 6. SEND PYOUT MESSAGE ON IOPUB
  ierl_message_sender:send_pyout(IOPubSocket, Result, [Session, Header, Date, ExeCount]).

  %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
  send_reply(
    UUID, Date, ParentHeader, Key,
    IOPubSocket, "status", idle, {}
  ).

handle_code_exception() ->
  %% TODO - each char of Traceback being output on separate line
  %% TODO - Traceback appears as a list of a million chars :/
  %% Leaving Traceback param as an empty list for now, and outputting via pyout
  %% reply_type, status, exe_count, excetpionName, ExceptionValue, TracebackList

  %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
  send_reply(
    UUID, Date, ParentHeader, Key,
    ShellSocket, "execute_reply", execute_reply_error, {"error", ExeCount, Exception, Reason, []}
  ).

  %%% 6. SEND PYOUT MESSAGE ON IOPUB
  ierl_message_sender:send_pyout(IOPubSocket, Result, [Session, Header, Date, ExeCount]).

  %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
  send_reply(
    UUID, Date, ParentHeader, Key,
    IOPubSocket, "status", idle, {}
  ).

handle_complete_request(Session, Date, Header, Key) ->
  case ierl_message_parser:parse(Content, complete_request) of
    {ok, _Text, _Line, _Block, _CursorPos} ->
      %% TODO - do something with complete_request
      print:line("TODO")
  end.

%%====================================================================
%% Message sending
%%====================================================================

send_reply(UUID, Date, ParentHeader, Key, Socket, HeaderType, ContentType, ContentArgs) ->
  NewHeader  = ierl_message_builder:generate_header(UUID, HeaderType, Date),
  NewContent = ierl_message_builder:generate_content(ContentType, ContentArgs),

  Data = {UUID, NewHeader, ParentHeader, NewContent},

  send_signed_reply(Socket, Data, Key).


send_signed_reply(Socket, Data, Key) ->
  {UUID, Header, ParentHeader, Content} = Data,

  Metadata = ierl_message_builder:generate_content(metadata),
  Hmac     = ierl_hmac:encode([Header, ParentHeader, Metadata, Content], Key),

  Reply = #reply_message{
    uuid          = UUID,
    hmac          = Hmac,
    header        = Header,
    parent_header = ParentHeader,
    metadata      = Metadata,
    content       = Content
  },

  ierl_message_sender:send_reply(Reply, Socket).
