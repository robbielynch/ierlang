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
-export ([start/2]).

-define(DEBUG, false).
-define(EXECUTE_REQUEST, "execute_request").
-define(EXECUTE_REPLY, "execute_reply").
-define(KERNEL_INFO_REQUEST, "kernel_info_request").
-define(KERNEL_INFO_REPLY, "kernel_info_reply").
-define(COMPLETE_REQUEST, "complete_request").
-define(STATUS, "status").
-define(OK_STATUS, "ok").
-define(ERROR_STATUS, "error").
-define(SUCCESS_MSG, "Successfully Compiled").

-record(reply_message, {
  uuid,                       % uuid
  delim = <<"<IDS|MSG>">>,    % Delimiter
  hmac =  <<"">>,             % HMAC
  header,                     % Serialized header
  parent_header,              % Serialized parent_header
  metadata = <<"{}">>,        % Serialized metadata
  content                     % Serialized content
}).


%%% @doc Starts the shell server
start(ShellSocket, IOPubSocket) ->
    ExeCount = 1,
    loop(ShellSocket, IOPubSocket, ExeCount).

loop(ShellSocket, IOPubSocket, ExeCount) ->
   shell_listener(ShellSocket, IOPubSocket, ExeCount, []),
   loop(ShellSocket, IOPubSocket, ExeCount).


%%% @doc Listens for messages on the Shell Socket, parses and
%%%      acts upon the message contents, then replies to IPython
shell_listener(ShellSocket, IOPubSocket, ExeCount, Bindings)->
    print("in shell listener"),
    {ok, UUID} = erlzmq:recv(ShellSocket),
    print("[Shell] Received UUID ",[UUID]),
    {ok, Delim} = erlzmq:recv(ShellSocket),
    print("[Shell] Received Delim ", [Delim]),
    {ok, Hmac} = erlzmq:recv(ShellSocket),
    print("[Shell] Received HMAC ", [Hmac]),
    {ok, Header} = erlzmq:recv(ShellSocket),
    print("[Shell] Received Header ", [Header]),
    {ok, ParentHeader} = erlzmq:recv(ShellSocket),
    print("[Shell] Received ParentHeader ", [ParentHeader]),
    {ok, Metadata} = erlzmq:recv(ShellSocket),
    print("[Shell] Received Metadata ", [Metadata]),
    {ok, Content} = erlzmq:recv(ShellSocket),
    print("[Shell] Received Content ", [Content]),

    case ierl_message_parser:parse_header([Header]) of
        %%% KERNEL_INFO_REQUEST
        {ok, _Username, Session, _MessageID, ?KERNEL_INFO_REQUEST, Date}->
      %%% KERNEL_INFO_REPLY
      KernelInfoReply = #reply_message{
        uuid = Session,
        parent_header = Header,
        header = ierl_message_builder:generate_header_reply(Session, ?KERNEL_INFO_REPLY, Date),
        content = ierl_message_builder:generate_content_reply(kernel_info_reply)
      },
      ierl_message_sender:send_reply(KernelInfoReply, ShellSocket);
        %%% EXECUTE_REQUEST
        {ok, _Username, Session, _MessageID, ?EXECUTE_REQUEST, Date}->
            %%% EXECUTE_REPLY STEPS:
      % 1. SEND BUSY STATUS ON IOPUB
      % 2. PARSE EXECUTE_REQUEST CONTENT
      % 3. SEND PYIN ON IOPUB
      % 4. EVALUATE THE ERLANG CODE
      % 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
      % 6. SEND PYOUT MESSAGE ON IOPUB
      % 7. SEND IDLE STATUS MESSAGE ON IOPUB

      %%% 1. SEND BUSY STATUS ON IOPUB
      BusyReplyRecord = #reply_message{
        uuid = Session,
        header = ierl_message_builder:generate_header_reply(Session, ?STATUS, Date),
        parent_header = Header,
        content = ierl_message_builder:generate_content_reply(busy)
      },
      ierl_message_sender:send_reply(BusyReplyRecord, IOPubSocket),

            %%% 2. PARSE EXECUTE_REQUEST CONTENT
            {ok, Code, _Silent, _StoreHistory, _UserVariables, _UserExpressions, _AllowStdin}
        = ierl_message_parser:parse_content(Content, execute_request),

      %%% 3. SEND PYIN ON IOPUB
      ierl_message_sender:send_pyin(IOPubSocket, Code, [Session, Header, Date]),

            %%% 4. EVALUATE THE ERLANG CODE
            case ierl_code_manager:module_or_expression(Code, Bindings) of
        %%%---------------------------------------------------------------
        %%% MODULE COMPILATION RESULTS
        %%% --------------------------------------------------------------
        {ok, CompileResultList}->
          case CompileResultList of
            [] -> CompileResult = ?SUCCESS_MSG;
            CompileMessage -> CompileResult = CompileMessage
          end,
          print("[Shell] Code Compile Result = ", [CompileResult]),

          %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
          print("[Shell] Generating execute content reply"),
          print("[Shell] Value = ", [CompileResult]),
          ExecuteReplyRecord = #reply_message{
            uuid = Session,
            header = ierl_message_builder:generate_header_reply(Session, ?EXECUTE_REPLY, Date),
            parent_header = Header,
            content = ierl_message_builder:generate_content_reply(execute_reply, {?OK_STATUS, ExeCount, {}, {}})
          },
          ierl_message_sender:send_reply(ExecuteReplyRecord, ShellSocket),

          %%% 6. SEND PYOUT MESSAGE ON IOPUB
          ierl_message_sender:send_pyout(IOPubSocket, CompileResult, [Session, Header, Date, ExeCount]),

          %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
          IdleStatusReplyRecord = #reply_message{
            uuid = Session,
            header = ierl_message_builder:generate_header_reply(Session, ?STATUS, Date),
            parent_header = Header,
            content = ierl_message_builder:generate_content_reply(idle)
          },
          ierl_message_sender:send_reply(IdleStatusReplyRecord, IOPubSocket),

          %%% Call Shell Listener again, incrementing the execution count by one.
          shell_listener(ShellSocket, IOPubSocket, ExeCount+1, Bindings);
        %%%----------------------------------------------------------------
        %%% SUCCESSFUL CODE EXECUTION
        %%%----------------------------------------------------------------
                {ok, Value, NewBindings}->
                    print("[Shell] Code Execution Result = ", [Value]),

          %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
          print("[Shell] Generating execute content reply"),
          print("[Shell] Value = ", [Value]),
          ExecuteReplyRecord = #reply_message{
            uuid = Session,
            header = ierl_message_builder:generate_header_reply(Session, ?EXECUTE_REPLY, Date),
            parent_header = Header,
            content = ierl_message_builder:generate_content_reply(execute_reply, {?OK_STATUS, ExeCount, {}, {}})
          },
          ierl_message_sender:send_reply(ExecuteReplyRecord, ShellSocket),

          %%% 6. SEND PYOUT MESSAGE ON IOPUB
          ierl_message_sender:send_pyout(IOPubSocket, Value, [Session, Header, Date, ExeCount]),

          %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
          IdleStatusReplyRecord = #reply_message{
            uuid = Session,
            header = ierl_message_builder:generate_header_reply(Session, ?STATUS, Date),
            parent_header = Header,
            content = ierl_message_builder:generate_content_reply(idle)
          },
          ierl_message_sender:send_reply(IdleStatusReplyRecord, IOPubSocket),

          %%% Call Shell Listener again, incrementing the execution count by one.
          shell_listener(ShellSocket, IOPubSocket, ExeCount+1, NewBindings);
        %%%--------------------------------------------------------------
        %% UNSUCCESSFUL CODE EXECUTION TODO - each char of Traceback being output on separate line
        %% --------------------------------------------------------------
                {error, Exception, Reason}->
                    print("[Shell] Code Execution Exception = ", [Exception]),
          print("Building error execute reply"),

          %% ERROR EXECUTE_REPLY %%TODO - Traceback appears as a list of a million chars :/
          %% Leaving Traceback param as an empty list for now, and outputting via pyout
          %% reply_type, status, exe_count, excetpionName, ExceptionValue, TracebackList
          %%% 5. SEND EXECUTE_REPLY MESSAGE ON SHELL SOCKET
          ExecuteReplyRecord = #reply_message{
            uuid = Session,
            header = ierl_message_builder:generate_header_reply(Session, ?EXECUTE_REPLY, Date),
            parent_header = Header,
            content = ierl_message_builder:generate_content_reply(execute_reply_error,
                                      {"error", ExeCount, Exception, Reason, []})
          },
          ierl_message_sender:send_reply(ExecuteReplyRecord, ShellSocket),

          %% PYERR MESSAGE
          %% ierl_message_sender:send_pyerr(IOPubSocket, Exception, ExeCount, Reason, [Traceback], [Session, Header, Date]),

          %%% 6. SEND PYOUT MESSAGE ON IOPUB
          ierl_message_sender:send_pyout(IOPubSocket, Reason, [Session, Header, Date, ExeCount]),

          %%% 7. SEND IDLE STATUS MESSAGE ON IOPUB
          IdleStatusReplyRecord = #reply_message{
            uuid = Session,
            header = ierl_message_builder:generate_header_reply(Session, ?STATUS, Date),
            parent_header = Header,
            content = ierl_message_builder:generate_content_reply(idle)
          },
          ierl_message_sender:send_reply(IdleStatusReplyRecord, IOPubSocket),

          shell_listener(ShellSocket, IOPubSocket, ExeCount+1, Bindings)
            end;
    %%%----------------------------------------------------------------
    %%% COMPLETE_REQUEST
    %%%----------------------------------------------------------------
    {ok, _Username, _Session, _MessageID, ?COMPLETE_REQUEST, _Date}->
      print("[Shell] Received complete_request message"),
      case ierl_message_parser:parse_content(Content, complete_request) of
        %%TODO - do something with complete_request
        {ok, _Text, _Line, _Block, _CursorPos} ->
          print("TODO");
        {error, Reason} ->
          print("[Shell] Error parsing complete_request - ", [Reason])
      end,
      shell_listener(ShellSocket, IOPubSocket, ExeCount, Bindings);
    {error, _Username, _Session, _MessageID, _UnexpectedMessageType, _Date} ->
      print("[Shell] Received unexpected message - " + [_UnexpectedMessageType]);
    {error, Reason} ->
      print("[Shell] Error occured trying to parse message - ", [Reason])

    end.

%% @doc Function to print stuff if debugging is set to true
print(Stuff)->
  case ?DEBUG of
    true ->  io:format("~p~n", [Stuff]);
    _Else -> "Do nothing"
  end.
print(Prompt, Stuff)->
  case ?DEBUG of
    true ->  io:format(string:concat(Prompt, "~p~n"), [Stuff]);
    _Else -> "Do nothing"
  end.

