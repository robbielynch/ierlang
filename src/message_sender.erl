%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, <COMPANY>
%%% @doc This module sends messages to IPython frontend using ZMQ
%%%
%%% @end
%%% Created : 03. Apr 2014 11:30
%%%-------------------------------------------------------------------
-module(message_sender).
-author("Robbie Lynch").
-export([send/4,send_pyout/3,send_pyin/3, send_pyerr/6, send_reply/2]).
-define(PYOUT, "pyout").
-define(PYIN, "pyin").
-define(PYERR, "pyerr").
-define(DISPLAY_DATA, "display_data").
-define(KERNEL_INFO_REPLY, "kernel_info_reply").

-record(reply_message, {
  uuid,
  delim = <<"<IDS|MSG>">>,
  hmac =  <<"">>,
  header,
  parent_header,
  metadata = <<"{}">>,
  content
}).

send_pyout(IOPubSocket, CodeOutput, [Session, IPythonHeader, Date, ExeCount])->
  PyoutReply = #reply_message{
    uuid = Session,
    header = message_builder:generate_header_reply(Session, ?PYOUT, Date),
    parent_header = IPythonHeader,
    content = message_builder:generate_content_reply(pyout, {ExeCount, CodeOutput})
  },
  send_reply(PyoutReply, IOPubSocket).

send_pyin(IOPubSocket, CodeInput, [Session, IPythonHeader, Date])->
  PyinReply = #reply_message{
    uuid = Session,
    header = message_builder:generate_header_reply(Session, ?PYIN, Date),
    parent_header = IPythonHeader,
    content = message_builder:generate_content_reply(pyin, {CodeInput, 1})
  },
  send_reply(PyinReply, IOPubSocket).

send_pyerr(IOPubSocket, ExceptionName, ExecutionCount, ExceptionVal,
                          Traceback, [Session, IPythonHeader, Date])->
  PyerrReply = #reply_message{
    uuid = Session,
    header = message_builder:generate_header_reply(Session, ?PYERR, Date),
    parent_header = IPythonHeader,
    content = message_builder:generate_content_reply(pyerr, {ExceptionName,
      ExecutionCount, ExceptionVal, Traceback})
  },
  send_reply(PyerrReply, IOPubSocket).

send(display_data, IOPubSocket, {Source, RawData, Metadata}, [Session, IPythonHeader, Date])->
  DisplayDataReply = #reply_message{
    uuid = Session,
    header = message_builder:generate_header_reply(Session, ?DISPLAY_DATA, Date),
    parent_header = IPythonHeader,
    content = message_builder:generate_content_reply(display_data, {Source, RawData, Metadata})
  },
  send_reply(DisplayDataReply, IOPubSocket).


send_reply(Record, Socket)->
  %% Send the Reply to IPython as a ZMQ multi-part message
  ok = erlzmq:send(Socket, list_to_binary(Record#reply_message.uuid), [sndmore]),
  ok = erlzmq:send(Socket, Record#reply_message.delim, [sndmore]),
  ok = erlzmq:send(Socket, Record#reply_message.hmac, [sndmore]),
  ok = erlzmq:send(Socket, list_to_binary(Record#reply_message.header), [sndmore]),
  ok = erlzmq:send(Socket, Record#reply_message.parent_header, [sndmore]),
  ok = erlzmq:send(Socket, Record#reply_message.metadata, [sndmore]),
  ok = erlzmq:send(Socket, list_to_binary(Record#reply_message.content)).
