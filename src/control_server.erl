%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc The Control server handles some requests from IPython
%%%
%%% @end
%%% Created : 31. Mar 2014 10:02
%%%-------------------------------------------------------------------
-module(control_server).
-author("Robbie Lynch").
-export([start/1]).

%%% @doc Start the control server
start(ControlSocket)->
  loop(ControlSocket).
  
loop(ControlSocket) ->
  control_listener(ControlSocket),
  loop(ControlSocket).

control_listener(ControlSocket)->
  {ok, Msg} = erlzmq:recv(ControlSocket),
  io:format("[Control] ~s~n", [Msg]).

%% TODO - Handle requests on the control server
%% control_responder(ControlSocket, Msg)->
%%   %io:format("[iopub] Keeping the dream alive"),
%%   ok = erlzmq:send(ControlSocket, Msg).