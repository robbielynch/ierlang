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

-export([parse/2]).

parse(Content, Type) ->
  {struct, PropList} = mochijson2:decode(Content),

  case Type of
    execute_request  -> parse_execute_request(PropList);
    complete_request -> parse_complete_request(PropList);
    header           -> parse_header(PropList)
  end.

parse_execute_request(PropList) ->
  Code            = binary_to_list(proplists:get_value(<<"code">>, PropList)),
  Silent          = proplists:get_value(<<"silent">>, PropList),
  StoreHistory    = proplists:get_value(<<"store_history">>, PropList),
  UserVariables   = proplists:get_value(<<"user_variables">>, PropList),
  UserExpressions = proplists:get_value(<<"user_expressions">>, PropList),
  AllowStdin      = proplists:get_value(<<"allow_stdin">>, PropList),

  {ok, Code, Silent, StoreHistory, UserVariables, UserExpressions, AllowStdin}.

parse_complete_request(PropList) ->
  Text      = binary_to_list(proplists:get_value(<<"text">>, PropList)),
  Line      = proplists:get_value(<<"line">>, PropList),
  Block     = proplists:get_value(<<"block">>, PropList),
  CursorPos = proplists:get_value(<<"cursor_pos">>, PropList),

  {ok, Text, Line, Block, CursorPos}.

parse_header(PropList) ->
  Username    = binary_to_list(proplists:get_value(<<"username">>, PropList)),
  Session     = binary_to_list(proplists:get_value(<<"session">>, PropList)),
  MessageID   = binary_to_list(proplists:get_value(<<"msg_id">>, PropList)),
  MessageType = binary_to_list(proplists:get_value(<<"msg_type">>, PropList)),

  {ok, Username, Session, MessageID, MessageType, ""}.
