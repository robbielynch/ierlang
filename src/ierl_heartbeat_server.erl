%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc The heartbeat sever keeps IPython alive by constantly
%%%      listening for messages on the given socket and replying
%%%      with a ping message.
%%% @end
%%% Created : 03. Apr 2014 11:51
%%%-------------------------------------------------------------------
-module(ierl_heartbeat_server).
-author("Robbie Lynch").
-export([start/1]).


%%% @doc Starts the heartbeat server
start(HeartbeatSocket) ->
    loop(HeartbeatSocket).

loop(HeartbeatSocket) ->
   heartbeat_listener(HeartbeatSocket),
   loop(HeartbeatSocket).

%%% @doc Heartbeat - this keeps IPython alive
%%%      by listening and replying to ping messages
heartbeat_listener(HeartbeatSocket)->
    {ok, Msg} = erlzmq:recv(HeartbeatSocket),
    %% Reply to IPython with a ping
    heartbeat_responder(HeartbeatSocket, Msg).
%%% @doc Sends a ping message to IPython via the Heartbeat Socket
heartbeat_responder(HeartbeatSocket, Msg)->
    %io:format("[Heartbeat] Keeping the dream alive"),
    ok = erlzmq:send(HeartbeatSocket, Msg).
