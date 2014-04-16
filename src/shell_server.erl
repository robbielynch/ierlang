%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2014 11:51
%%%-------------------------------------------------------------------
-module (shell_server).
-author("Robbie Lynch").

%% API
-export ([start/2]).


start(ShellSocket, IOPubSocket) ->
    ExeCount = 1,
    loop(ShellSocket, IOPubSocket, ExeCount).

loop(ShellSocket, IOPubSocket, ExeCount) ->
   shell_listener(ShellSocket, IOPubSocket, ExeCount, []),
   loop(ShellSocket, IOPubSocket, ExeCount).


%% Shell Socket - This interacts with iPython
%% The shell listener listens for messages from 
%% IPython. Parses the message and determines
%% what IPython wants us to do next.
shell_listener(ShellSocket, IOPubSocket, ExeCount, Bindings)->
	io:format("in shell listener~n"),
	%%%io:format("[Shell] In shell_listener ~n"),
	{ok, UUID} = erlzmq:recv(ShellSocket),
	io:format("[Shell] Received UUID  ~s~n", [UUID]),
	{ok, Delim} = erlzmq:recv(ShellSocket),
	io:format("[Shell] Received Delim ~s~n", [Delim]),
	{ok, Hmac} = erlzmq:recv(ShellSocket),
	io:format("[Shell] Received HMAC ~s~n", [Hmac]),
	{ok, Header} = erlzmq:recv(ShellSocket),
	io:format("[Shell] Received Header  ~s~n", [Header]),
	{ok, ParentHeader} = erlzmq:recv(ShellSocket),
	io:format("[Shell] Received ParentHeader ~s~n", [ParentHeader]),
	{ok, Metadata} = erlzmq:recv(ShellSocket),
	io:format("[Shell] Received Metadata ~s~n", [Metadata]),
	{ok, Content} = erlzmq:recv(ShellSocket),
	io:format("[Shell] Received Content ~s~n~n", [Content]),

	case message_parser:parse_header([Header]) of
		%% KERNEL_INFO_REQUEST
		%% IPython requests information about the kernel.
		%% python version, language name, messaging version, ipython version
		{ok, _Username, Session, _MessageID, "kernel_info_request", Date}->
			ReplyHeader = message_builder:generate_header_reply(Session, "kernel_info_reply", Date),
			ReplyContent = message_builder:generate_content_reply(kernel_info_reply),
			Metadata = message_builder:create_metadata(),
			ReplyList = message_builder:msg_parts_to_ipython_msg(Session, ReplyHeader,
                                           Header, ReplyContent, Metadata),
			% SendReply
			message_sender:send_reply(ReplyList, ShellSocket);

		%% EXECUTE_REQUEST
		{ok, _Username, Session, _MessageID, "execute_request", Date}->
			%%EXECUTE_REPLY
			ExecuteReplyHeader = message_builder:generate_header_reply(Session, "execute_reply", Date),
			Metadata = message_builder:create_metadata(),

			%% BUSY STATUS MESSAGE
			%%io:format("[Shell] Sending busy status to IPython ~n"),
			BusyReplyHeader = message_builder:generate_header_reply(Session, "status", Date),
			BusyStatusReplyContent = message_builder:generate_content_reply(busy),
			%%io:format("[Shell] Busy Status Reply Content =  ~s~n", [BusyStatusReplyContent]),
			BusyStatusReplyList = message_builder:msg_parts_to_ipython_msg(Session, BusyReplyHeader,
                                                Header, BusyStatusReplyContent, Metadata),
			message_sender:send_reply(BusyStatusReplyList, IOPubSocket),

			%% Parse the EXECUTE_REQUEST content
			{ok, Code, _Silent, _StoreHistory, _UserVariables, _UserExpressions, _AllowStdin}
        = message_parser:parse_content(Content, execute_request),
			%%io:format("[Shell] Executing this code:  ~s~n", [Code]),

      %% PYIN
      %send_pyin(IOPubSocket, Code, [MessageID, Session, Username, Header, Date]),

			%% EVALUATE ERLANG CODE
			case code_manager:module_or_expression(Code, Bindings) of
        %% Module Compilation Results--------------------------------------------------------------
        {ok, CompileResultList}->
          case CompileResultList of
            [] -> CompileResult = "Successfully Compiled";
            CompileMessage -> CompileResult = CompileMessage
          end,
          io:format("[Shell] Code Compile Result = ~p~n", [CompileResult]),

          %% EXECUTE_REPLY MESSAGE
          %% reply_type, status, exe_count, payload, user_vars, user_exprs
          io:format("[Shell] Generating execute content reply~n"),
          io:format("[Shell] Value = ~p~n", [CompileResult]),
          ReplyContent = message_builder:generate_content_reply(execute_reply, {"ok", ExeCount, {}, {}}),
          ReplyList = message_builder:msg_parts_to_ipython_msg(Session, ExecuteReplyHeader, Header, ReplyContent, Metadata),
          message_sender:send_reply(ReplyList, ShellSocket),

          %% PYOUT MESSAGE
          message_sender:send_pyout(IOPubSocket, CompileResult, [Session, Header, Date, ExeCount]),

          %% IDLE STATUS MESSAGE
          %%io:format("[Shell] Sending IDLE status to IPython ~n"),
          IdleReplyHeader = message_builder:generate_header_reply(Session, "status", Date),
          IdleStatusReplyContent = message_builder:generate_content_reply(idle),
          %%io:format("[Shell] IDLE Status Reply Content =  ~s~n", [IdleStatusReplyContent]),
          IdleStatusReplyList = message_builder:msg_parts_to_ipython_msg(Session, IdleReplyHeader, Header,
            IdleStatusReplyContent, Metadata),
          message_sender:send_reply(IdleStatusReplyList, IOPubSocket),

          %% Call Shell Listener again, incrementing the execution count by one.
          shell_listener(ShellSocket, IOPubSocket, ExeCount+1, Bindings);
        %% Successful Code Execution----------------------------------------------------------------
				{ok, Value, NewBindings}->
					io:format("[Shell] Code Execution Result = ~p~n", [Value]),

					%% EXECUTE_REPLY MESSAGE
          io:format("[Shell] Generating execute content reply~n"),
          io:format("[Shell] Value = ~p~n", [Value]),
					ReplyContent = message_builder:generate_content_reply(execute_reply, {"ok", ExeCount, {}, {}}),
					ReplyList = message_builder:msg_parts_to_ipython_msg(Session, ExecuteReplyHeader, Header, ReplyContent, Metadata),
					message_sender:send_reply(ReplyList, ShellSocket),

          %% PYOUT MESSAGE
          message_sender:send_pyout(IOPubSocket, Value, [Session, Header, Date, ExeCount]),

					%% IDLE STATUS MESSAGE
					%%io:format("[Shell] Sending IDLE status to IPython ~n"),
					IdleReplyHeader = message_builder:generate_header_reply(Session, "status", Date),
					IdleStatusReplyContent = message_builder:generate_content_reply(idle),
					%%io:format("[Shell] IDLE Status Reply Content =  ~s~n", [IdleStatusReplyContent]),
					IdleStatusReplyList = message_builder:msg_parts_to_ipython_msg(Session, IdleReplyHeader, Header,
                                                          IdleStatusReplyContent, Metadata),
					message_sender:send_reply(IdleStatusReplyList, IOPubSocket),

          %% Call Shell Listener again, incrementing the execution count by one.
          shell_listener(ShellSocket, IOPubSocket, ExeCount+1, NewBindings);
        %% Unsuccessful Code Execution TODO - not working------------------------------------------
				{error, Exception, Reason}->
					io:format("[Shell] Code Execution Exception = ~p~n", [Exception]),
          erlang:display("Building error execute reply"),

          %% PYERR MESSAGE
          %ExceptionName, ExecutionCount, ExceptionValue, Traceback
          message_sender:send_pyerr(IOPubSocket, Exception, ExeCount, Reason, [], [Session, Header, Date]),

          %% ERROR EXECUTE_REPLY
          %% reply_type, status, exe_count, excetpionName, ExceptionValue, TracebackList
          ErrorReplyContent = message_builder:generate_content_reply(execute_reply_error, {"error", ExeCount, Exception, Reason, []}),
          erlang:display("Generated error reply content"),
          ReplyList = message_builder:msg_parts_to_ipython_msg(Session, ExecuteReplyHeader, Header,
                                                ErrorReplyContent, Metadata),
          erlang:display("Sending execute_reply error"),
          message_sender:send_reply(ReplyList, ShellSocket),

          %% IDLE STATUS MESSAGE
          %%io:format("[Shell] Sending IDLE status to IPython ~n"),
          IdleReplyHeader = message_builder:generate_header_reply(Session, "status", Date),
          IdleStatusReplyContent = message_builder:generate_content_reply(idle),
          %%io:format("[Shell] IDLE Status Reply Content =  ~s~n", [IdleStatusReplyContent]),
          IdleStatusReplyList = message_builder:msg_parts_to_ipython_msg(Session, IdleReplyHeader, Header,
                                                        IdleStatusReplyContent, Metadata),
          message_sender:send_reply(IdleStatusReplyList, IOPubSocket),
          shell_listener(ShellSocket, IOPubSocket, ExeCount+1, Bindings)
			end;
    %%COMPLETE_REQUEST
    {ok, _Username, _Session, _MessageID, "complete_request", _Date}->
      io:format("[Shell] Received complete_request message"),
      case message_parser:parse_content(Content, complete_request) of
        %%TODO - do something with complete_request
        {ok, _Text, _Line, _Block, _CursorPos} ->
          io:format("TODO");
        {error, Reason} ->
          io:format("[Shell] Error parsing complete_request - ~s~n", [Reason])
      end,
      shell_listener(ShellSocket, IOPubSocket, ExeCount+1, Bindings);
    {error, _Username, _Session, _MessageID, _UnexpectedMessageType, _Date} ->
      io:format("[Shell] Received unexpected message - ~s~n", [_UnexpectedMessageType]);
    {error, Reason} ->
      io:format("[Shell] Error occured trying to parse message - ~p~n", [Reason])

	end.