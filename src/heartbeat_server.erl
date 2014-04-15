%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2014 11:51
%%%-------------------------------------------------------------------
-module(heartbeat_server).
-author("Robbie Lynch").

%% API
-export([start/1]).

start(HeartbeatSocket) ->
    loop(HeartbeatSocket).

loop(HeartbeatSocket) ->
   heartbeat_listener(HeartbeatSocket),
   loop(HeartbeatSocket).


%% Heartbeat - this keeps IPython alive
%% The hearbeat listener receives ping messages
%% from IPython
heartbeat_listener(HeartbeatSocket)->
    {ok, Msg} = erlzmq:recv(HeartbeatSocket),
    %% Reply to IPython with a ping
    heartbeat_responder(HeartbeatSocket, Msg).
%% Sends a ping message to IPython via the Heartbeat Socket
heartbeat_responder(HeartbeatSocket, Msg)->
    %io:format("[Heartbeat] Keeping the dream alive"),
    ok = erlzmq:send(HeartbeatSocket, Msg).