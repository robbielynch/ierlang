%%%-------------------------------------------------------------------
%%% @author Roberto Aloi
%%% @doc Erlang Sanbox to run erlang code in a restricted environment
%%% @end
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% THE FOLLOWING MODULE WAS WRITTEN BY ROBERT ALOI, WHO MADE HIS ERLANG
%%% SANDBOX CODE AVAILABLE ON GITHUB @ https://github.com/robertoaloi/erlang-sandbox
%%%-------------------------------------------------------------------
-module(restrictions).

-export([is_allowed/2, is_allowed/3]).

-define(MAX_HEAP_SIZE, 10000).
-define(MAX_ARGS_SIZE, 200).
-define(MAX_SEQ, 100).

-define(LOCAL_ALLOWED, [
  %% Shell commands
  f,
  %% Erlang module
  round,
  atom_to_list,
  binary_to_list,
  integer_to_list,
  float_to_list,
  tuple_to_list,
  list_to_binary,
  list_to_integer,
  list_to_tuple,
  list_to_existing_atom,
  setelement,
  element,
  size,
  split_binary,
  error,
  throw,
  time,
  date,
  now,
  universaltime,
  localtime,
  localtime_to_universaltime
]).

-define(NON_LOCAL_ALLOWED, [
  lists,
  string,
  re,
  sets,
  sofs,
  ordsets,
  dict,
  orddict,
  gb_sets,
  gb_trees,
  calendar,
  queue,
  proplists,
  math,
  {os, [
    type,
    version
  ]},
  io_lib,
  {erlang, [
    '+',
    '-',
    '*',
    '/',
    'bnot',
    'div',
    'rem',
    'band',
    'bor',
    'bxor',
    'bsl',
    'bsr',
    'not',
    'and',
    'or',
    'xor',
    '==',
    '/=',
    '=<',
    '<',
    '>=',
    '>',
    '=:=',
    '=/=',
    '++',
    '--',
    round,
    atom_to_list,
    binary_to_list,
    integer_to_list,
    float_to_list,
    tuple_to_list,
    list_to_binary,
    list_to_integer,
    list_to_tuple,
    list_to_existing_atom,
    setelement,
    element,
    size,
    split_binary,
    error,
    throw,
    time,
    date,
    now,
    universaltime,
    localtime,
    localtime_to_universaltime
  ]}]).

is_allowed(Function, Args) ->
  case proplists:lookup(Function, ?LOCAL_ALLOWED) of
    {Function, true} ->
      check_limits(Args);
    _Else ->
      false
  end.

is_allowed(io_lib, get_until, _) ->
  false;
is_allowed(lists, duplicate, [{integer, 1, Int} | _]) ->
  Int < ?MAX_SEQ;
is_allowed(lists, duplicate, _) ->
  false;
is_allowed(lists, seq, [{integer, 1, From}, {integer, 1, To} | _]) ->
  abs(From - To) < ?MAX_SEQ;
is_allowed(lists, seq, _) ->
  false;
is_allowed(Module, Function, Args) ->
  case proplists:lookup(Module, ?NON_LOCAL_ALLOWED) of
    {Module, true} ->
      check_limits(Args);
    {Module, Functions} when is_list(Functions) ->
      case lists:member(Function, Functions) of
        true ->
          check_limits(Args);
        false ->
          false
      end;
    _Else ->
      false
  end.

check_limits(Args) ->
  max_args(Args) andalso max_heap_size().

max_args(Args) ->
  ArgsSize = erts_debug:flat_size(Args),
  case ArgsSize < ?MAX_ARGS_SIZE of
    true ->
      true;
    false ->
      sandbox:restricted_msg()
  end.

max_heap_size() ->
  {heap_size, HeapSize} = erlang:process_info(self(), heap_size),
  case HeapSize < ?MAX_HEAP_SIZE of
    true ->
      true;
    false ->
      sandbox:restricted_msg()
  end.