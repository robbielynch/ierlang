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

send_pyout(IOPubSocket, CodeOutput, [Session, IPythonHeader, Date, ExeCount])->
  %%Build and send Pyout reply
  ReplyHeader = message_builder:generate_header_reply(Session, ?PYOUT, Date),
  Metadata = message_builder:create_metadata(),
  PyoutContentReply = message_builder:generate_content_reply(pyout, {ExeCount, CodeOutput}),
  PyoutReplyList = message_builder:msg_parts_to_ipython_msg(Session,
                                    ReplyHeader, IPythonHeader, PyoutContentReply, Metadata),
  %%io:format("[IOPub] Sending pyout... ~n"),
  send_reply(PyoutReplyList, IOPubSocket).

send_pyin(IOPubSocket, CodeInput, [Session, IPythonHeader, Date])->
  %%Build and send pyin reply
  ReplyHeader = message_builder:generate_header_reply(Session, ?PYIN, Date),
  Metadata = message_builder:create_metadata(),
  PyinContentReply = message_builder:generate_content_reply(pyin, {CodeInput, 1}),
  PyinReplyList = message_builder:msg_parts_to_ipython_msg(Session,
                                ReplyHeader, IPythonHeader, PyinContentReply, Metadata),
  send_reply(PyinReplyList, IOPubSocket).

send_pyerr(IOPubSocket, ExceptionName, ExecutionCount, ExceptionVal,
    Traceback, [Session, IPythonHeader, Date])->
  %%Build and send pyin reply
  ReplyHeader = message_builder:generate_header_reply(Session, ?PYERR, Date),
  Metadata = message_builder:create_metadata(),
  PyerrContentReply = message_builder:generate_content_reply(pyerr, {ExceptionName,
                                          ExecutionCount, ExceptionVal, Traceback}),
  PyerrReplyList = message_builder:msg_parts_to_ipython_msg(Session,
                            ReplyHeader, IPythonHeader, PyerrContentReply, Metadata),
  send_reply(PyerrReplyList, IOPubSocket).

send(display_data, IOPubSocket, {Source, RawData, Metadata}, [Session, IPythonHeader, Date])->
  ReplyHeader = message_builder:generate_header_reply(Session, ?DISPLAY_DATA, Date),
  DisplayDataContentReply = message_builder:generate_content_reply(display_data, {Source, RawData, Metadata}),
  DisplayDataReplyList = message_builder:msg_parts_to_ipython_msg(Session,
                              ReplyHeader, IPythonHeader, DisplayDataContentReply, Metadata),
  send_reply(DisplayDataReplyList, IOPubSocket);

send(kernel_info_reply, ShellSocket, {}, [Session, Header, Date])->
  ReplyHeader = message_builder:generate_header_reply(Session, ?KERNEL_INFO_REPLY, Date),
  ReplyContent = message_builder:generate_content_reply(kernel_info_reply),
  Metadata = message_builder:create_metadata(),
  ReplyList = message_builder:msg_parts_to_ipython_msg(Session, ReplyHeader,
    Header, ReplyContent, Metadata),
  % SendReply
  message_sender:send_reply(ReplyList, ShellSocket).


send_reply([UUIDs, Delim, HMAC, Header, ParentHeader, Metadata, Content], Socket)->
  %% Send the Reply to IPython as a ZMQ multi-part message
  ok = erlzmq:send(Socket, UUIDs, [sndmore]),
  ok = erlzmq:send(Socket, Delim, [sndmore]),
  ok = erlzmq:send(Socket, HMAC, [sndmore]),
  ok = erlzmq:send(Socket, Header, [sndmore]),
  ok = erlzmq:send(Socket, ParentHeader, [sndmore]),
  ok = erlzmq:send(Socket, Metadata, [sndmore]),
  ok = erlzmq:send(Socket, Content).