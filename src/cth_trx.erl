-module(cth_trx).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/4]).
-export([post_init_per_group/5]).
-export([pre_end_per_group/4]).
-export([post_end_per_group/5]).

-export([pre_init_per_testcase/4]).
-export([post_init_per_testcase/5]).
-export([pre_end_per_testcase/4]).
-export([post_end_per_testcase/5]).

-export([on_tc_fail/4]).
-export([on_tc_skip/4]).

-export([terminate/1]).

-record(state, {path, filename}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
   make_ref().

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, Opts) ->
    {ok, #state{path=proplists:get_value(path, Opts, "_build/test"),
                filename=proplists:get_value(filename, Opts)}}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Suite,_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Suite,_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each end_per_group.
pre_end_per_group(_Suite,_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(_Suite,_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_testcase.
pre_init_per_testcase(_Suite,_TC,Config,State) ->
    {Config, State}.

%% Called after each init_per_testcase (immediately before the test case).
post_init_per_testcase(_Suite,_TC,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each end_per_testcase (immediately after the test case).
pre_end_per_testcase(_Suite,_TC,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_testcase.
post_end_per_testcase(_Suite,_TC,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(_Suite, _TC, _Reason, State) ->
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.
on_tc_skip(_Suite, _TC, _Reason, State) ->
    State.

%% @doc Called when the scope of the CTH is done
terminate(#state{filename=File}) ->
    %% We are operating *under* the right profile already
    {ok, Cwd} = file:get_cwd(),
    Path = filename:join(lists:droplast(lists:droplast(filename:split(Cwd)))),
    case trx_ct:convert(Path) of
        {error, no_report_found} ->
            io:format(user, "no common test run logs found under current run in ~p~n", [Path]);
        {ok, RunName, XML} ->
            FileName = case File of
                undefined -> filename:join([Path, RunName ++ ".trx"]);
                ArgFile -> filename:join([Path, ArgFile])
            end,
            ok = file:write_file(FileName, XML),
            io:format(user, "TRX Report written at ~p~n", [FileName])
    end.
