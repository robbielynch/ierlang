%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc This modules defines functions used to starts all the
%%%      IErlang servers, to handle messages, and to parse the json
%%%      configuration file.
%%%
%%% @end
%%% Created : 31. Mar 2014 10:02
%%%-------------------------------------------------------------------
-module (ierl_script).
-export ([main/1]).

%% JSON FILE CONTENTS
%% Receives a list of arguments from IPython.
%% The list contains the absolute path to the kernel.json file.
%% The kernel.json file contains:
%%      stdin_port: 52248,
%%      ip: "127.0.0.1",
%%      control_port: 52249,
%%      hb_port: 52250,
%%      signature_scheme: "hmac-sha256",
%%      key: "",
%%      shell_port: 52246,
%%      transport: "tcp",
%%      iopub_port: 52247

main(JsonFile) ->
  io:format("[ERLANG KERNEL]~n"),
  erlang:display(JsonFile),
  %% Read json file
  {ok, _File} = file:read_file(JsonFile),
  JsonContent = readlines(JsonFile),
  case parse_json_file(JsonContent) of
    {ok, StdInPort, IP, ControlPort, HbPort, _SignatureScheme, _Key, ShellPort, Transport, IOPubPort}->
      % Start the zmq manager - which creates and binds all sockets.
      % The zmq manager starts all necessary servers to handle messaging
      ierl_zmq_manager:run([{hbport, HbPort}, {shellport, ShellPort}, {controlport, ControlPort},
        {iopubport, IOPubPort}, {stdinport, StdInPort}, {ip, IP}, {transport, Transport}]);
    {error, Execption, Reason}->
      io:format("[ERLANG KERNEL] ERROR STARTING ERLANG KERNEL - ~p~p~n", [Execption,Reason])
  end.

%%% @doc Parses the IPython configuration file and extracts all necessary information.
parse_json_file(Json)->
  try
    {struct, JsonData} = mochijson2:decode(Json),
    StdInPort = proplists:get_value(<<"stdin_port">>, JsonData),
    IP = proplists:get_value(<<"ip">>, JsonData),
    ControlPort = proplists:get_value(<<"control_port">>, JsonData),
    HbPort = proplists:get_value(<<"hb_port">>, JsonData),
    SignatureScheme = proplists:get_value(<<"signature_scheme">>, JsonData),
    Key = proplists:get_value(<<"key">>, JsonData),
    ShellPort = proplists:get_value(<<"shell_port">>, JsonData),
    Transport = proplists:get_value(<<"transport">>, JsonData),
    IOPubPort = proplists:get_value(<<"iopub_port">>, JsonData),
    {ok, StdInPort, IP, ControlPort, HbPort, SignatureScheme, Key, ShellPort, Transport, IOPubPort}
  catch
    Exception:Reason ->
      E = io_lib:format("~p", [Exception]),
      R = io_lib:format("~p", [Reason]),
      {error, E, R}
  end.

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

