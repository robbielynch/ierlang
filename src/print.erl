-module(print).

-define(DEBUG, true).

-export([
  line/1,
  line/2
]).

line(Line) ->
  case ?DEBUG of
    true -> io:format("~p~n", [Line]);
    _    -> ok
  end.

line(Text, Term) ->
  case ?DEBUG of
    true -> io:format(Text ++ "~p~n", [Term]);
    _    -> ok
  end.
