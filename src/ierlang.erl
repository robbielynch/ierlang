-module(ierlang).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ierlang_sup:start_link().

stop(_State) ->
    ok.
