%%%-------------------------------------------------------------------
%% @doc logger_test top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(logger_test_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, test/0, do_test/0, do_lager/1, do_erlogger/1]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],

    ok = logger:add_handler(?MODULE, logger_std_h,
        #{config => #{file => "/var/log/logger_test/logger.erl.log"}}),

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions


test() ->
    Refs = [spawn_monitor(?MODULE, do_test, [])],
    wait_down(Refs).


wait_down([]) ->
    ok;
wait_down([_H | Refs]) ->
    receive
        {'DOWN', MonitorRef, Type, Object, Info} ->
            lager:warning("process ~p is DOWN",[{MonitorRef, Type, Object, Info}]),
            wait_down(Refs)
    end.

do_test() ->

    {DebugTime1,ok} = timer:tc(?MODULE, do_lager, [debug]),
    {WarnTime1,ok} = timer:tc(?MODULE, do_lager, [warning]),
    timer:sleep(1000),
    io:format("~n --------  Lager: ~pms / ~pms --------- ~n",[DebugTime1, WarnTime1]),

    {DebugTime2,ok} = timer:tc(?MODULE, do_erlogger, [debug]),
    {WarnTime2,ok} = timer:tc(?MODULE, do_erlogger, [warning]),
    timer:sleep(1000),
    io:format("~n --------  Erlang Logger: ~pms / ~pms --------- ~n",[DebugTime2, WarnTime2]),

    ok.

do_lager(Level) ->
    lager:set_loglevel(lager_file_backend, Level),
    lager:warning("Set level ~p",[Level]),
    [lager:debug("test ~p",[N]) || N <- lists:seq(1,100) ],
    ok.

do_erlogger(Level) ->

    %%logger:update_handler_config(?MODULE, config, #{level => Level}),
    logger:set_application_level(logger_test, Level),

    [ logger:debug("test ~p",[N]) || N <- lists:seq(1,100) ],
    ok.
