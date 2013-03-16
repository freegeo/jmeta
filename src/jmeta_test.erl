%% Author: Said
%% Created: 19.02.2013
%% Description: TODO: Add description to jmeta_test
-module(jmeta_test).

%%
%% Include files
%%

-define(DONE_KEY, {ok, done}).

%%
%% Exported Functions
%%

-export([run/0, done/0]).

%%
%% API Functions
%%

run() ->
    Modules =
        [list_to_atom(filename:basename(Name, ".beam")) ||
           Name <- filelib:wildcard([filename:dirname(code:which(?MODULE)), "/*.beam"])],
    ModulesWithTest =
        [Module ||
         Module <- Modules,
         lists:member({test, 0}, Module:module_info(exports)),
         Module =/= jmeta],
    case [R || R <- [check(M) || M <- ModulesWithTest], R =/= ?DONE_KEY] of
        [] -> integration_test();
        Errors -> {error, Errors}
    end.

done() ->
    ?DONE_KEY.

%%
%% Local Functions
%%

check(M) ->
    try
        {ok, done} = M:test()
    catch
        _:Why -> [{module, M}, {reason, Why}, {stack, erlang:get_stacktrace()}]
    end.

% TODO
integration_test() ->
    {ok, complete}.
