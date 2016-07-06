%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc This module sends messages to IPython frontend using ZMQ
%%%
%%% @end
%%% Created : 03. Apr 2014 11:30
%%%-------------------------------------------------------------------

-module(ierl_message_sender).
-author("Robbie Lynch").

-define(PYOUT,             "pyout"            ).
-define(PYIN,              "pyin"             ).
-define(PYERR,             "pyerr"            ).
-define(DISPLAY_DATA,      "display_data"     ).
-define(KERNEL_INFO_REPLY, "kernel_info_reply").

-record(reply_message, {
  uuid,                       % uuid
  delim = <<"<IDS|MSG>">>,    % Delimiter
  hmac =  <<"">>,             % HMAC
  header,                     % Serialized header
  parent_header,              % Serialized parent_header
  metadata = <<"{}">>,        % Serialized metadata
  content                     % Serialized content
}).

-export([
  send/4,
  send_pyerr/6,
  send_pyin/3,
  send_pyout/3,
  send_reply/2
]).

%%% @doc Send a pyout message
send_pyout(IOPubSocket, CodeOutput, [Session, IPythonHeader, Date, ExeCount])->
  PyoutReply = #reply_message{
    uuid          = Session,
    header        = ierl_message_builder:generate_header(Session, ?PYOUT, Date),
    parent_header = IPythonHeader,
    content       = ierl_message_builder:generate_content(pyout, {ExeCount, CodeOutput})
  },

  send_reply(PyoutReply, IOPubSocket).

%%% @doc Send a pyin message
send_pyin(IOPubSocket, CodeInput, [Session, IPythonHeader, Date])->
  PyinReply = #reply_message{
    uuid          = Session,
    header        = ierl_message_builder:generate_header(Session, ?PYIN, Date),
    parent_header = IPythonHeader,
    content       = ierl_message_builder:generate_content(pyin, {CodeInput, 1})
  },

  send_reply(PyinReply, IOPubSocket).

%%% @doc Send a pyerr message
send_pyerr(IOPubSocket, ExceptionName, ExecutionCount, ExceptionVal,
                          Traceback, [Session, IPythonHeader, Date])->
  PyerrReply = #reply_message{
    uuid          = Session,
    header        = ierl_message_builder:generate_header(Session, ?PYERR, Date),
    parent_header = IPythonHeader,
    content       = ierl_message_builder:generate_content(pyerr, {ExceptionName,
      ExecutionCount, ExceptionVal, Traceback})
  },

  send_reply(PyerrReply, IOPubSocket).

%%% @doc Send a display_data message
send(display_data, IOPubSocket, {Source, RawData, Metadata}, [Session, IPythonHeader, Date])->
  DisplayDataReply = #reply_message{
    uuid          = Session,
    header        = ierl_message_builder:generate_header(Session, ?DISPLAY_DATA, Date),
    parent_header = IPythonHeader,
    content       = ierl_message_builder:generate_content(display_data, {Source, RawData, Metadata})
  },

  send_reply(DisplayDataReply, IOPubSocket).

%%% @doc Send the reply message record to IPython on the given socket
send_reply(Record, Socket)->
  %% Send the Reply to IPython as a ZMQ multi-part message
  ok = erlzmq:send(Socket, list_to_binary(Record#reply_message.uuid),   [sndmore]),
  ok = erlzmq:send(Socket, Record#reply_message.delim,                  [sndmore]),
  ok = erlzmq:send(Socket, Record#reply_message.hmac,                   [sndmore]),
  ok = erlzmq:send(Socket, list_to_binary(Record#reply_message.header), [sndmore]),
  ok = erlzmq:send(Socket, Record#reply_message.parent_header,          [sndmore]),
  ok = erlzmq:send(Socket, Record#reply_message.metadata,               [sndmore]),
  ok = erlzmq:send(Socket, list_to_binary(Record#reply_message.content)          ),

  ok.
