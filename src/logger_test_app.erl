%%%-------------------------------------------------------------------
%% @doc logger_test public API
%% @end
%%%-------------------------------------------------------------------

-module(logger_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
