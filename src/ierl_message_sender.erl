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
  send_by_shell/5,
  send_by_io/5
]).

%%====================================================================
%% API functions
%%====================================================================

send_by_shell(State, Request, HeaderType, ContentType, ContentArgs) ->
  Data = format_data(Request, HeaderType, ContentType, ContentArgs),

  send_signed_reply(
    State#shell_state.shell_socket,
    Data,
    State#shell_state.key
  ).

send_by_io(State, Request, HeaderType, ContentType, ContentArgs) ->
  Data = format_data(Request, HeaderType, ContentType, ContentArgs),

  send_signed_reply(
    State#shell_state.io_pub_socket,
    Data,
    State#shell_state.key
  ).

%%====================================================================
%% Internal functions
%%====================================================================

format_data(Request, HeaderType, ContentType, ContentArgs) ->
  ParsedHeader = Request#request.parsed_header,
  UUID         = ParsedHeader#parsed_header.uuid,
  Date         = ParsedHeader#parsed_header.date,
  NewHeader    = ierl_message_builder:generate_header(UUID, HeaderType, Date),
  NewContent   = ierl_message_builder:generate_content(ContentType, ContentArgs),

  {UUID, NewHeader, Request#request.header, NewContent}.

send_signed_reply(Socket, Data, Key) ->
  {UUID, Header, ParentHeader, Content} = Data,

  Metadata = ierl_message_builder:generate_content(metadata, {}),
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
