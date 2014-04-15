%%%-------------------------------------------------------------------
%%% @author robbie
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2014 10:02
%%%-------------------------------------------------------------------
-module(control_server).
-author("Robbie Lynch").

%% API
-export([start/1]).


start(ControlSocket)->
  loop(ControlSocket).
  
loop(ControlSocket) ->
  control_listener(ControlSocket),
  loop(ControlSocket).


control_listener(ControlSocket)->
  {ok, Msg} = erlzmq:recv(ControlSocket),
  io:format("[Control] ~s~n", [Msg]).

%% control_responder(ControlSocket, Msg)->
%%   %io:format("[iopub] Keeping the dream alive"),
%%   ok = erlzmq:send(ControlSocket, Msg).