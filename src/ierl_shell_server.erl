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

-include("records.hrl").

-export ([start/3]).

%%====================================================================
%% API functions
%%====================================================================

%%% @doc Starts the shell server
start(ShellSocket, IOPubSocket, Key) ->
  State = #shell_state{
    shell_socket    = ShellSocket,
    io_pub_socket   = IOPubSocket,
    key             = Key,
    bindings        = [],
    execution_count = 1
  },

  loop(State).

%%====================================================================
%% Internal functions
%%====================================================================

loop(State) ->
  NewState = handle_new_request(State),

  loop(NewState).

handle_new_request(State) ->
  Request      = receive_request(State#shell_state.shell_socket),
  ParsedHeader = Request#request.parsed_header,

  case ParsedHeader#parsed_header.message_type of
    "kernel_info_request" -> handle_kernel_info_request(State, Request);
    "execute_request"     -> handle_execute_request(State, Request);
    "complete_request"    -> handle_complete_request(State, Request)
  end.

receive_request(ShellSocket) ->
  {ok, _UUID}         = erlzmq:recv(ShellSocket),
  {ok, _Delim}        = erlzmq:recv(ShellSocket),
  {ok, _Hmac}         = erlzmq:recv(ShellSocket),
  {ok, Header}        = erlzmq:recv(ShellSocket),
  {ok, _ParentHeader} = erlzmq:recv(ShellSocket),
  {ok, _Metadata}     = erlzmq:recv(ShellSocket),
  {ok, Content}       = erlzmq:recv(ShellSocket),

  ParsedHeader = ierl_message_parser:parse([Header], header),

  #request{
    header         = Header,
    content        = Content,
    parsed_header  = ParsedHeader
  }.

handle_kernel_info_request(State, Request) ->
  ierl_message_sender:send(State, Request, "kernel_info_reply", kernel_info_reply, {}),

  State.

handle_execute_request(State, Request) ->
  % 1. SEND BUSY STATUS ON IOPUB
  % 2. PARSE EXECUTE_REQUEST CONTENT
  % 3. SEND PYIN ON IOPUB
  % 4. EVALUATE THE ERLANG CODE
  % 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
  % 6. SEND PYOUT MESSAGE ON IOPUB
  % 7. SEND IDLE STATUS MESSAGE ON IOPUB

  %%% 1. SEND BUSY STATUS ON IOPUB
  ierl_message_sender:send(State, Request, "status", busy, {}),

  %%% 2. PARSE EXECUTE_REQUEST CONTENT
  ParsedContent = ierl_message_parser:parse(Content, execute_request),

  %%% 3. SEND PYIN ON IOPUB
  ierl_message_sender:send_pyin(State, Request, Result),

  %%% 4. EVALUATE THE ERLANG CODE
  CodeEvaluation = ierl_code_manager:module_or_expression(State, ParsedContent),

  case CodeEvaluation of
    {ok, CompileResultList}    -> handle_compilation_result(State, Request, CompileResultList);
    {ok, Value, NewBindings}   -> handle_code_execution(State, Request, Value, NewBindings);
    {error, Exception, Reason} -> handle_code_exception(State, Request, Exception, Reason)
  end.

handle_compilation_result(State, Request, CompileResultList) ->
  CompiledResult = case CompileResultList of
    []             -> "Successfully Compiled";
    CompileMessage -> CompileMessage
  end,

  %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
  ierl_message_sender:send(State, Request, "execute_reply", execute_reply, {"ok", ExeCount, {}, {}}),

  %%% 6. SEND PYOUT MESSAGE ON IOPUB
  ierl_message_sender:send_pyout(State, Request, Result),

  %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
  ierl_message_sender:send(State, Request, "status", idle, {}),

  State.

handle_code_execution(State) ->
  %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
  ierl_message_sender:send(State, Request, "execute_reply", execute_reply, {"ok", ExeCount, {}, {}}),

  %%% 6. SEND PYOUT MESSAGE ON IOPUB
  ierl_message_sender:send_pyout(State, Request, Result),

  %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
  ierl_message_sender:send(State, Request, "status", idle, {}),

  State.

handle_code_exception(State) ->
  %% TODO - each char of Traceback being output on separate line
  %% TODO - Traceback appears as a list of a million chars :/
  %% Leaving Traceback param as an empty list for now, and outputting via pyout
  %% reply_type, status, exe_count, excetpionName, ExceptionValue, TracebackList

  %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
  ierl_message_sender:send(State, Request, "execute_reply", execute_reply_error, {"error", ExeCount, Exception, Reason, []}),

  %%% 6. SEND PYOUT MESSAGE ON IOPUB
  ierl_message_sender:send_pyout(State, Request, Result),

  %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
  ierl_message_sender:send(State, Request, "status", idle, {}),

  State.

handle_complete_request(State, Request) ->
  case ierl_message_parser:parse(Content, complete_request) of
    {ok, _Text, _Line, _Block, _CursorPos} ->
      %% TODO - do something with complete_request
      print:line("TODO")
  end.
