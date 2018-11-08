-module(trx_ct).
-mode(compile).

-export([convert/1]).

-include_lib("xmerl/include/xmerl.hrl").

convert(TestPath) ->
    case file:read_file(filename:join([TestPath, "logs", "ct.latest.log"])) of
        {error, enoent} ->
            {error, no_report_found};
        {error, Term} ->
            {error, Term};
        {ok, LatestLog} ->
            RunDir = find_run_dir(LatestLog),
            LogDirs = filelib:wildcard(filename:join([RunDir, "*.logs"])),
            LogRunDirs = lists:append([filelib:wildcard(filename:join([Dir, "run.*"]))
                                    || Dir <- LogDirs]),
            SuiteLogs = [{Dir, parse(Log)}
                        || Dir <- LogRunDirs,
                           {ok, Log} <- [file:read_file(filename:join([Dir, "suite.log"]))]],
            SuiteOutput = [{Global, [get_output(Dir, Case) || Case <- Cases]}
                        || {Dir, {Global, Cases}} <- SuiteLogs],
            RunName = filename:basename(RunDir),
            XML = generate_trx(SuiteOutput, RunDir),
            {ok, RunName, XML}
    end.

find_run_dir(Log) ->
    [Line] = [Line || <<"CWD set to: ", Line/binary>> <- string:split(Log, "\n", all)],
    unicode:characters_to_list(string:trim(Line, both, "\"")).

parse(Log) ->
    Data = [list_to_tuple(re:split(string:trim(Line), " +", [{parts,2}, unicode]))
            || Line <- re:split(Log, "^=", [unicode, multiline]),
                nomatch =:= string:prefix(Line, "=="),
                not string:equal(Line, "")],
    group_log_data([case T of
                        {K} -> {K, <<"">>}; % default
                        {K,V} -> {K, V}
                    end || T <- Data]).


generate_trx(Data, RunDir) ->
    {Global, AllCases} = merge_data(Data),
    #{user := User, domain := Domain, started := Start, finished := Finish,
      cases := Cases, successful := Passed, failed := Failed,
      auto_skipped := ASkip, user_skipped := USkip, computer_name := Computer
     } = Global,
    TestListId = "19431567-8539-422a-85d7-44ee4e166bda",
    TestTypeId = "13cdc9d9-ddb5-4fa4-a97d-d965ccfc6d4b",
    {ok, Cwd} = file:get_cwd(),
    {{Y,M,D},{H,Mi,S}} = calendar:local_time(),
    SimpleXML = 
    {'TestRun',
        [{id, uuid:uuid_to_string(uuid:get_v4())},
         {name, [User, "@", Domain, " Generated: ",
                 io_lib:format("~p:~p:~p ~p:~p:~p", [Y,M,D,H,Mi,S])]},
         {runUser, [Domain, "/", User]},
         {xmlns, "http://microsoft.com/schemas/VisualStudio/TeamTest/2010"}],
        [{'TestSettings',
          [{name, "Default"}, {id, uuid:uuid_to_string(uuid:get_v4())}],
          [{'Description', [], ["Default settings."]},
           {'Deployment', 
            [{userDeploymentRoot, Cwd}, {useDefaultDeploymentRoot, "false"},
             {runDeploymentRoot, RunDir}],
            [{'DeploymentItem', [{filename,"rebar.config"}], []}]},
           {'Execution', [],
            [{'Timeouts', [{runTimeout, 57600000}], []},
             {'TestTypeSpecific', [], [
              {'UnitTestRunConfig', [{testTypeId, TestTypeId}],
               [{'AssemblyResolution', [], [{'TestDirectory', [{useLoadContext, "true"}], []}]}]}
             ]},
             {'AgentRule', [{name, "LocalMachineDefaultRole"}], []}
            ]}
          ]},
         {'Times', [{start, Start}, {finish, Finish}, {creation, Start}],[]},
         {'ResultSummary', [{outcome, outcome({Passed, Failed, ASkip, USkip})}],
          [{'Counters',
           [{total,Cases}, {executed,Cases}, {passed,Passed}, {error,0}, {failed,Failed+ASkip},
            {timeout,0}, {aborted,0}, {inconclusive,0}, {passedButRunAborted,0},
            {notRunnable,USkip}, {notExecuted,0}, {disconnected,0}, {warning,USkip},
            {completed, 0}, {inProgress,0}, {pending, 0}],
           []},
           {'RunInfos', [], []}]},
          {'TestDefinitions', [], xml_tests(Global, AllCases)},
          {'TestLists', [], [{'TestList', [{name, "All Loaded Results"}, {id, TestListId}], []}]},
          {'TestEntries', [], 
           [{'TestEntry',
             [{testId, Id}, {executionId, Id}, {testListId, TestListId}],
             []}
           || #{id := Id} <- AllCases]},
          {'Results', [], xml_results(AllCases, {TestListId, TestTypeId, Computer})}
        ]
    },
    unicode:characters_to_list(xmerl:export_simple([SimpleXML], xmerl_xml)).

merge_data(List) ->
    Default = #{
        cases => 0, successful => 0, failed => 0, auto_skipped => 0, user_skipped => 0,
        started => <<"9999-01-01 00:00:00">>, finished => <<"1970-01-01 00:00:00">>,
        domain => os:getenv("USERDOMAIN", "localhost"),
        computer_name => os:getenv("COMPUTERNAME", "localhost")
    },
    lists:foldl(fun({G=#{cases := GC, successful := GP, failed := GF,
                         auto_skipped := GA, user_skipped := GU,
                         started := GS, finished := GE}, D},
                    {GAcc=#{cases := AC, successful := AP, failed := AF,
                            auto_skipped := AA, user_skipped := AU,
                            started := AS, finished := AE}, DAcc}) ->
        {(maps:merge(G,GAcc))#{
            cases := binary_to_integer(GC)+AC, successful := binary_to_integer(GP)+AP,
            failed := binary_to_integer(GF)+AF, auto_skipped := binary_to_integer(GA)+AA,
            user_skipped := binary_to_integer(GU)+AU, started := min(GS,AS), finished := max(GE,AE)
         },
         D ++ DAcc}
    ; (_, Acc) -> % incomplete results
        Acc
    end, {Default,[]}, List).

group_log_data(Data) ->
    {Global, Rest} = group_global(Data),
    Cases = group_cases(Rest),
    {Global, Cases}.

group_global(Data) ->
    %% Grab all until 'case' entry
    Head = lists:takewhile(fun({Key,_}) -> not string:equal(Key, "case") end, Data),
    %% Last 5 elements are also global
    [T5,T4,T3,T2,T1|_] = lists:reverse(Data),
    Global = Head++[T1,T2,T3,T4,T5],
    {maps:from_list(convert_keys(Global)), Data -- Global}.

group_cases(Data) -> group_cases(Data, [], []).

group_cases([], _, []) -> [];
group_cases([], _, [H|T]) -> lists:reverse(T, [maps:from_list(add_id(convert_keys(H)))]);
group_cases([{<<"case">>, Name}=H|T], Groups, Acc) ->
    IsInitPerGroup = string:find(Name, "init_per_group", trailing) =/= nomatch,
    IsEndPerGroup = string:find(Name, "end_per_group", trailing) =/= nomatch,
    IsCT = string:find(Name, "init_per_suite", trailing) =/= nomatch orelse
           string:find(Name, "end_per_suite", trailing) =/= nomatch orelse
           IsInitPerGroup orelse IsEndPerGroup,
    Next = lists:dropwhile(fun({K,_}) -> not string:equal(K, "case") end, T),
    if IsInitPerGroup ->
        [{_, Props}|_] = lists:dropwhile(fun({K,_}) -> not string:equal(K, "group_props") end, T),
        {match, [GroupName]} = re:run(Props, "{name,(.*)}", [unicode, {capture, all_but_first, binary}]),
        group_cases(Next, Groups ++ [GroupName], Acc)
    ;  IsEndPerGroup ->
        group_cases(Next, lists:droplast(Groups), Acc)
    ;  IsCT ->
        group_cases(Next, Groups, Acc)
    ;  not IsCT andalso Acc =:= [] ->
        group_cases(T, Groups, [[H, {<<"groups">>, Groups}]])
    ;  not IsCT andalso Acc =/= [] ->
        [C|Cs] = Acc,
        group_cases(T, Groups, [[H, {<<"groups">>, Groups}],
                    maps:from_list(add_id(convert_keys(C))) | Cs])
    end;
group_cases([H|T], Groups, [C|Cs]) ->
    group_cases(T, Groups, [[H|C]|Cs]).

convert_keys(List) ->
    [{binary_to_atom(K, utf8), V} || {K,V} <- List].

add_id(List) ->
    [{id, uuid:uuid_to_string(uuid:get_v4())} | List].

%% @private This is very hacky
get_output(Dir, M=#{logfile := Filename}) ->
    {ok, Log} = file:read_file(filename:join([Dir, Filename])),
    {match, Output} = re:run(Log, "=== Started at.*<br />(.*)<a name=\"end\"",
                             [global,dotall,multiline,unicode, {capture, all_but_first, list}]),
    M#{output => clean(Output)}.

%% @private This is very hacky
clean(HtmlLog) ->
    Cleaned = re:replace(HtmlLog,
                         "</?(div|pre|b)( class=\"[a-z_]+\")?>",
                          "", [unicode, global]),
    %% drop stacktrace link if any
    Raw = re:replace(Cleaned, <<"<a href=\"#end\">.*</a>">>, "", [unicode, {return, list}]),
    string:trim(Raw).

outcome({_,0,0,_}) -> "Passed"; % full test run, {Pass, Fail, Skip:Auto, Skip:User}
outcome(<<"ok">>) -> "Passed";
outcome(<<"skipped", _Rest/binary>>) -> "Warning";     % user controlled skip
outcome(<<"auto_skipped", _Rest/binary>>) -> "Failed";  % likely an init_per_xxx crash
outcome(_) -> "Failed".

xml_tests(_Global, Cases) ->
    [xml_testcase(Case) || Case <- Cases].

xml_testcase(#{'case' := Case, id := Id, groups := Groups}) ->
    [Mod,Test] = string:lexemes(Case, ":"),
    BeamPath = find_path(Mod),
    TestName = lists:join(".", Groups ++ [Test]),
    FullTestName = [Mod,":",TestName],
    {'UnitTest',
     [{name, FullTestName}, {id, Id}],
     [{'TestCategory',[],
        [{'TestCategoryItem',[{'TestCategory', Group}],[] } || Group <- Groups]},
      {'Execution', [{id, Id}], []},
      {'TestMethod',
       [{codeBase, BeamPath}, {name, TestName},
        {className, "ct"}],
       []}
     ]}.
    
%% @private cache path finding per module since this is costly as an operation
find_path(RawMod) ->
    Mod = unicode:characters_to_list(RawMod),
    case get({path_cache, Mod}) of
        undefined ->
            Path = case code:where_is_file(Mod ++ ".beam") of
                    non_existing -> "C:/path/unknown/from/test/logs";
                    Str -> Str
                   end,
            put({path_cache, Mod}, Path),
            Path;
        Path ->
            Path
    end.

xml_results(Cases, Args) ->
    [xml_result(Case, Args) || Case <- Cases].

xml_result(#{'case' := Case, id := Id, started := Started, ended := Ended,
              result := Result, output := Output, groups := Groups},
              {TestListId, TestTypeId, Computer}) ->
    [Mod,Test] = string:lexemes(Case, ":"),
    TestName = [Mod,":",lists:join(".", Groups ++ [Test])],
    {'UnitTestResult',
     [{executionId, Id}, {testId, Id}, {testName, TestName}, {testListId, TestListId},
     {startTime, format_date(Started)}, {endTime, format_date(Ended)},
     {outcome, outcome(Result)},
     {computerName, Computer}, {testType, TestTypeId}],
      [{'Output', [],
        [{'StdOut', [], [Output || Output =/= ""]}]}]}.

format_date(StrDate) ->
    [Date, Time] = string:lexemes(StrDate, " "),
    [Date, "T", Time, ".0000000-05:00"].
