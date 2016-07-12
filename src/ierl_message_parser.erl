%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc Module to parse IPython messages and their contents
%%%
%%% @end
%%% Created : 03. Apr 2014 10:15
%%%-------------------------------------------------------------------
-module(ierl_message_parser).
-author("Robbie Lynch").

-include("records.hrl").

-export([parse/2]).

parse(Content, Type) ->
  {struct, PropList} = mochijson2:decode(Content),

  case Type of
    execute_request  -> parse_execute_request(PropList);
    complete_request -> parse_complete_request(PropList);
    header           -> parse_header(PropList)
  end.

parse_execute_request(PropList) ->
  #parsed_code{
    code             = binary_to_list(proplists:get_value(<<"code">>, PropList)),
    silent           = proplists:get_value(<<"silent">>, PropList),
    store_history    = proplists:get_value(<<"store_history">>, PropList),
    user_variables   = proplists:get_value(<<"user_variables">>, PropList),
    user_expressions = proplists:get_value(<<"user_expressions">>, PropList),
    allow_stdin      = proplists:get_value(<<"allow_stdin">>, PropList)
  }.

parse_complete_request(PropList) ->
  #parsed_text{
    text       = binary_to_list(proplists:get_value(<<"text">>, PropList)),
    line       = proplists:get_value(<<"line">>, PropList),
    block      = proplists:get_value(<<"block">>, PropList),
    cursor_pos = proplists:get_value(<<"cursor_pos">>, PropList)
  }.

parse_header(PropList) ->
  #parsed_header{
    username     = binary_to_list(proplists:get_value(<<"username">>, PropList)),
    uuid         = binary_to_list(proplists:get_value(<<"session">>, PropList)),
    message_id   = binary_to_list(proplists:get_value(<<"msg_id">>, PropList)),
    message_type = binary_to_list(proplists:get_value(<<"msg_type">>, PropList))
  }.
