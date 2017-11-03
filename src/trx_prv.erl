-module(trx_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, trx).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 trx"},      % How to use the plugin
            {opts, [                      % list of options understood by the plugin
                {file, $f, "file", undefined, "name of the .trx file to generate"}
            ]},
            {short_desc, "A rebar plugin to convert test output to visual studio's trx format"},
            {desc, "A rebar plugin to convert test output to visual studio's trx format"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    BuildDir = rebar_dir:base_dir(State),
    {Args, _} = rebar_state:command_parsed_args(State),
    rebar_api:info("Processing common test logs...", []),
    case trx_ct:convert(BuildDir) of
        {error, Term} ->
            {error, {?MODULE, {ct, Term}}};
        {ok, RunName, XML} ->
            FileName = case proplists:get_value(file, Args) of
                undefined -> filename:join([BuildDir, RunName ++ ".trx"]);
                ArgFile -> filename:join([BuildDir, ArgFile])
            end,
            ok = file:write_file(FileName, XML),
            rebar_api:info("Report written at ~p", [FileName]),
            {ok, State}
    end.

-spec format_error(any()) ->  iolist().
format_error({ct, no_report_found}) ->
    io_lib:format("no common test run logs found under current profile.", []);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
