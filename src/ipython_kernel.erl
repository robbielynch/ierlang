#!escript

%%
%% Receives a list of arguments from IPython.
%% The list contains the absolute path to the kernel.json file.
%% The kernel.json file contains:
%% 		stdin_port: 52248, 
%% 		ip: "127.0.0.1", 
%% 		control_port: 52249, 
%% 		hb_port: 52250, 
%% 		signature_scheme: "hmac-sha256", 
%% 		key: "", 
%% 		shell_port: 52246, 
%% 		transport: "tcp", 
%% 		iopub_port: 52247
%%

main([JsonFile|_]) ->
  io:format("[ERLANG KERNEL]~n"),
  % lists:foreach(fun(Arg) -> io:format("Got argument: ~p~n", [Arg]) end,Args).
  %file:write_file("erlang_kernel_log.txt", "hello from erlang kernel\n", [append]),
  erlang:display(JsonFile),
  % Load file
  {ok, _File} = file:read_file(JsonFile),
  % Read all lines in the file to get its content.
  JsonContent = readlines(JsonFile),
  % mochijson2:decode and extract the proplists from the Json string
  {struct, JsonData} = mochijson2:decode(JsonContent),
  % Extract each variable from the proplists
  StdInPort = proplists:get_value(<<"stdin_port">>, JsonData),
  IP = proplists:get_value(<<"ip">>, JsonData),
  ControlPort = proplists:get_value(<<"control_port">>, JsonData),
  HbPort = proplists:get_value(<<"hb_port">>, JsonData),
  SignatureScheme = proplists:get_value(<<"signature_scheme">>, JsonData),
  Key = proplists:get_value(<<"key">>, JsonData),
  ShellPort = proplists:get_value(<<"shell_port">>, JsonData),
  Transport = proplists:get_value(<<"transport">>, JsonData),
  IOPubPort = proplists:get_value(<<"iopub_port">>, JsonData),
  % Output everything that's been extracted from the proplists
  IPythonInfoList = [StdInPort, IP, ControlPort, HbPort, SignatureScheme, Key, ShellPort, Transport, IOPubPort],
  erlang:display(IPythonInfoList),
  erlang:display(IP),
  % Start the zmq poller - which creates and binds all sockets.
  % The zmq poller constantly listens on these ports.
  zmq_manager:run([{hbport, HbPort}, {shellport, ShellPort}, {controlport, ControlPort},
             {iopubport, IOPubPort}, {stdinport, StdInPort}, {ip, IP}, {transport, Transport}]).

readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> [];
    Line -> Line ++ get_all_lines(Device)
  end.

