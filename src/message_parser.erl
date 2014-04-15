%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2014 10:15
%%%-------------------------------------------------------------------
-module(message_parser).
-author("Robbie Lynch").

%% API
-export([parse_content/2, parse_header/1]).


%% @spec parse_content(list(), atom()) -> tuple()
%% @doc Parses and extracts the Content items from the IPython execute_request message
parse_content(Content, execute_request)->
  try
  {struct, MsgPropList} = mochijson2:decode(Content),
  Code = binary_to_list(proplists:get_value(<<"code">>, MsgPropList)),
  Silent = proplists:get_value(<<"silent">>, MsgPropList),
  StoreHistory = proplists:get_value(<<"store_history">>, MsgPropList),
  UserVariables = proplists:get_value(<<"user_variables">>, MsgPropList),
  UserExpressions = proplists:get_value(<<"user_expressions">>, MsgPropList),
  AllowStdin = proplists:get_value(<<"allow_stdin">>, MsgPropList),
  {ok, Code, Silent, StoreHistory, UserVariables, UserExpressions, AllowStdin}
  catch
    _:Reason ->
      io:format("[Message Parser] Error parsing execute_request"),
      {error, Reason}
  end;

%% @spec parse_content(list(), atom()) -> tuple()
%% @doc Parses and extracts the Content items from the IPython complete_request message
parse_content(Content, complete_request)->
  try
    {struct, MsgPropList} = mochijson2:decode(Content),
    Text = binary_to_list(proplists:get_value(<<"text">>, MsgPropList)),
    Line = proplists:get_value(<<"line">>, MsgPropList),
    Block = proplists:get_value(<<"block">>, MsgPropList),
    CursorPos = proplists:get_value(<<"cursor_pos">>, MsgPropList),
    {ok, Text, Line, Block, CursorPos}
  catch
    _:Reason ->
      io:format("[Message Parser] Error parsing complete_request"),
      {error, Reason}
  end.

%% @spec parse_header(list()) -> tuple()
%% @doc Parses and extracts the header items from the IPython header message.
parse_header(Header)->
  try
    {struct, HeaderPropList} = mochijson2:decode(Header),
    %Date = extract_date_from_header(HeaderPropList),
    Username = binary_to_list(proplists:get_value(<<"username">>, HeaderPropList)),
    Session = binary_to_list(proplists:get_value(<<"session">>, HeaderPropList)),
    MessageID = binary_to_list(proplists:get_value(<<"msg_id">>, HeaderPropList)),
    MessageType = binary_to_list(proplists:get_value(<<"msg_type">>, HeaderPropList)),
    io:format("[MessageParser] Message Type Received = ~s~n", [MessageType]),
    {ok, Username, Session, MessageID, MessageType, ""}
  catch
    _:Reason ->
      io:format("~p: ~p~n", ["[MessageParser] Error parsingIpythonMessage", Reason]),
      {error, Reason}
  end.


