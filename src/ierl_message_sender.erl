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

-include("records.hrl").

-export([
  send/4,
  send_busy/2,
  send_pyerr/6,
  send_pyin/3,
  send_pyout/3,
]).

%%====================================================================
%% API functions
%%====================================================================

send(State, Request, ContentType, ContentArgs) ->
  ParsedHeader = Request#request.parsed_header,
  NewHeader    = ierl_message_builder:generate_header(UUID, HeaderType, Date),
  NewContent   = ierl_message_builder:generate_content(ContentType, ContentArgs),

  Data = {UUID, NewHeader, ParentHeader, NewContent},

  send_signed_reply(Socket, Data, Key).

send_busy(State, Request) ->


%%% @doc Send a pyerr message
send_pyerr(IOPubSocket, ExceptionName, ExecutionCount, ExceptionVal,
                          Traceback, [Session, IPythonHeader, Date])->
  PyerrReply = #reply_message{
    uuid          = Session,
    header        = ierl_message_builder:generate_header(Session, "pyerr", Date),
    parent_header = IPythonHeader,
    content       = ierl_message_builder:generate_content(pyerr, {ExceptionName,
      ExecutionCount, ExceptionVal, Traceback})
  },

  send_reply(PyerrReply, IOPubSocket).

%%% @doc Send a pyin message
send_pyin(IOPubSocket, CodeInput, [Session, IPythonHeader, Date])->
  PyinReply = #reply_message{
    uuid          = Session,
    header        = ierl_message_builder:generate_header(Session, "pyin", Date),
    parent_header = IPythonHeader,
    content       = ierl_message_builder:generate_content(pyin, {CodeInput, 1})
  },

  send_reply(PyinReply, IOPubSocket).

%%% @doc Send a pyout message
send_pyout(IOPubSocket, CodeOutput, [Session, IPythonHeader, Date, ExeCount])->
  PyoutReply = #reply_message{
    uuid          = Session,
    header        = ierl_message_builder:generate_header(Session, "pyout", Date),
    parent_header = IPythonHeader,
    content       = ierl_message_builder:generate_content(pyout, {ExeCount, CodeOutput})
  },

  send_reply(PyoutReply, IOPubSocket).

%%====================================================================
%% Internal functions
%%====================================================================

send_signed_reply(Socket, Data, Key) ->
  {UUID, Header, ParentHeader, Content} = Data,

  Metadata = ierl_message_builder:generate_content(metadata),
  Hmac     = ierl_hmac:encode([Header, ParentHeader, Metadata, Content], Key),

  Reply = #reply_message{
    uuid          = UUID,
    hmac          = Hmac,
    header        = Header,
    parent_header = ParentHeader,
    metadata      = Metadata,
    content       = Content
  },

  send_reply(Reply, Socket).

%%% @doc Send the Reply to IPython as a ZMQ multi-part message
send_reply(Reply, Socket)->
  ok = erlzmq:send(Socket, list_to_binary(Reply#reply_message.uuid),   [sndmore]),
  ok = erlzmq:send(Socket, Reply#reply_message.delim,                  [sndmore]),
  ok = erlzmq:send(Socket, Reply#reply_message.hmac,                   [sndmore]),
  ok = erlzmq:send(Socket, list_to_binary(Reply#reply_message.header), [sndmore]),
  ok = erlzmq:send(Socket, Reply#reply_message.parent_header,          [sndmore]),
  ok = erlzmq:send(Socket, Reply#reply_message.metadata,               [sndmore]),
  ok = erlzmq:send(Socket, list_to_binary(Reply#reply_message.content)          ),

  ok.
