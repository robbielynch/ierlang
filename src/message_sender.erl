%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2014 11:30
%%%-------------------------------------------------------------------
-module(message_sender).
-author("Robbie Lynch").

%% API
-export([send_pyout/3,send_pyin/3, send_pyerr/6, send_reply/2]).

send_pyout(IOPubSocket, CodeOutput, [Session, IPythonHeader, Date, ExeCount])->
  %%Build and send Pyout reply
  ReplyHeader = message_builder:generate_header_reply(Session, "pyout", Date),
  Metadata = message_builder:create_metadata(),
  PyoutContentReply = message_builder:generate_content_reply(pyout, {ExeCount, CodeOutput}),
  PyoutReplyList = message_builder:msg_parts_to_ipython_msg(Session,
                                    ReplyHeader, IPythonHeader, PyoutContentReply, Metadata),
  %%io:format("[IOPub] Sending pyout... ~n"),
  send_reply(PyoutReplyList, IOPubSocket).

send_pyin(IOPubSocket, CodeInput, [Session, IPythonHeader, Date])->
  %%Build and send pyin reply
  ReplyHeader = message_builder:generate_header_reply(Session, "pyin", Date),
  Metadata = message_builder:create_metadata(),
  PyinContentReply = message_builder:generate_content_reply(pyin, {CodeInput, 1}),
  PyinReplyList = message_builder:msg_parts_to_ipython_msg(Session,
                                ReplyHeader, IPythonHeader, PyinContentReply, Metadata),
  %%io:format("[IOPub] Sending Pyin... ~n"),
  send_reply(PyinReplyList, IOPubSocket).
send_pyerr(IOPubSocket, ExceptionName, ExecutionCount, ExceptionVal,
    Traceback, [Session, IPythonHeader, Date])->
  %%Build and send pyin reply
  ReplyHeader = message_builder:generate_header_reply(Session, "pyerr", Date),
  Metadata = message_builder:create_metadata(),
  PyerrContentReply = message_builder:generate_content_reply(pyerr, {ExceptionName,
                                          ExecutionCount, ExceptionVal, Traceback}),
  PyerrReplyList = message_builder:msg_parts_to_ipython_msg(Session,
                            ReplyHeader, IPythonHeader, PyerrContentReply, Metadata),
  %%io:format("[IOPub] Sending Pyin... ~n"),
  send_reply(PyerrReplyList, IOPubSocket).


send_reply([UUIDs, Delim, HMAC, Header, ParentHeader, Metadata, Content], Socket)->
  %% Send the Reply to IPython as a ZMQ multi-part message
  ok = erlzmq:send(Socket, UUIDs, [sndmore]),
  ok = erlzmq:send(Socket, Delim, [sndmore]),
  ok = erlzmq:send(Socket, HMAC, [sndmore]),
  ok = erlzmq:send(Socket, Header, [sndmore]),
  ok = erlzmq:send(Socket, ParentHeader, [sndmore]),
  ok = erlzmq:send(Socket, Metadata, [sndmore]),
  ok = erlzmq:send(Socket, Content).