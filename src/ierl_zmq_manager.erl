%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, <COMPANY>
%%% @doc Set up all the ZMQ sockets and bindings, also spawns the servers
%%%      that handle the messaging. Once this is done it goes into an
%%%      infinite empty loop to keep IErlang process alive.
%%%
%%% @end
%%% Created : 03. Apr 2014 11:51
%%%-------------------------------------------------------------------
-module (ierl_zmq_manager).
-author("Robbie Lynch").
-export([run/1]).

%% @doc Function that creates and binds all zmq sockets. Starts the heartbeat, shell and control servers.
run(
  [
    {hbport,           HbPort         },
    {signature_scheme, SignatureScheme},
    {key,              Key            },
    {shellport,        ShellPort      },
    {controlport,      ControlPort    },
    {iopubport,        IOPubPort      },
    {stdinport,        StdInPort      },
    {ip,               IP             },
    {transport,        Transport      }
  ]
) ->
  ConnectionStringBuilder1 = string:concat(binary_to_list(Transport),"://"),
  ConnectionStringBuilder2 = string:concat(binary_to_list(IP),":"),
  ConnectionString         = string:concat(ConnectionStringBuilder1, ConnectionStringBuilder2),

    %% Create Sockets that will be used to communicate with IPython
    {ok, Context}         = erlzmq:context(),
    {ok, HeartbeatSocket} = erlzmq:socket(Context, rep),
    {ok, ControlSocket}   = erlzmq:socket(Context, router),
    {ok, StdinSocket}     = erlzmq:socket(Context, router),
    {ok, ShellSocket}     = erlzmq:socket(Context, router),
    {ok, IOPubSocket}     = erlzmq:socket(Context, pub),

    %% Bind Sockets to Ports
  ok = erlzmq:bind(HeartbeatSocket, string:concat(ConnectionString, integer_to_list(HbPort))),
  ok = erlzmq:bind(ControlSocket, string:concat(ConnectionString, integer_to_list(ControlPort))),
  ok = erlzmq:bind(StdinSocket, string:concat(ConnectionString, integer_to_list(StdInPort))),
  ok = erlzmq:bind(ShellSocket, string:concat(ConnectionString, integer_to_list(ShellPort))),
  ok = erlzmq:bind(IOPubSocket, string:concat(ConnectionString, integer_to_list(IOPubPort))),

  % Start the heartbeat, shell and controll servers
  spawn(ierl_heartbeat_server, start, [HeartbeatSocket]              ),
  spawn(ierl_shell_server,     start, [ShellSocket, IOPubSocket, Key]),
  spawn(ierl_control_server,   start, [ControlSocket]                ),

  %% Constantly listen and reply to messages
  loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket).

%% Function to listen and respond on all sockets.
%% Constant loop keeps the kernel alive.
loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket) ->
  %% Keep listening and responding
  timer:sleep(5),

  loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket).
