%% Author: Said
%% Mailto: said.dk@gmail.com
%% Created: 18.02.2013
%% Published under MIT license.
-module(jmeta_namespace).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("jmeta.hrl").
-include("jtils.hrl").
-include("jtest.hrl").

%% --------------------------------------------------------------------
-export([test/0]).

%% External exports
-export([start_link/1, add/1, delete/1, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

test() ->
    test_api(),
    test_behaviour(),
    jmeta_test:done().

start_link(Namespace) ->
    gen_server:start_link({local, ?NAMESPACE(Namespace)}, ?MODULE, [], []).

add(Meta) ->
    case jmeta_declaration:parse(Meta) of
        {error, _} = E -> E;
        Result ->
            Namespace = jmeta_declaration:namespace(Result),
            case exists(Namespace) of
                false -> new(Namespace);
                true -> dont_care
            end,
            call(Namespace, {add, {jmeta_declaration:key(Result), Result}})
    end.

delete({Namespace, _} = Key) ->
    case call(Namespace, {delete, Key}) of
        0 -> release(Namespace), 0;
        Count -> Count
    end.

get({Namespace, _} = Key) ->
    call(Namespace, {get, Key}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, dict:new()}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get, Key}, _, State) ->
    {reply, case dict:find(Key, State) of
                {ok, Data} -> Data;
                error -> {error, {Key, is_not_defined}}
            end, State};
handle_call({add, {Key, Record}}, _, State) ->
    New = dict:store(Key, Record, State),
    {reply, dict:size(New), New};
handle_call({delete, Key}, _, State) ->
    New = dict:erase(Key, State),
    {reply, dict:size(New), New};
handle_call(_, _, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_, _) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_, State, _) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

new(Namespace) ->
    supervisor:start_child(jmeta_sup, [Namespace]).

release(Namespace) ->
    supervisor:terminate_child(jmeta_sup, whereis(?NAMESPACE(Namespace))).

exists(Namespace) ->
    whereis(?NAMESPACE(Namespace)) =/= undefined.

call(Namespace, Request) ->
    case exists(Namespace) of
        true -> gen_server:call(?NAMESPACE(Namespace), Request);
        false -> {error, {wrong_namespace, Namespace}}
    end.

test_api() ->
    TestTypes =
        [case Meta of
             {type, Name, Data} -> {type, ?TN(Name), Data};
             {frame, Name, Data} -> {frame, ?TN(Name), Data}
         end || Meta <- lists:sublist(jmeta_library:std(), 3)],
    First = lists:nth(1, TestTypes),
    Second = lists:nth(2, TestTypes),
    Third = lists:nth(3, TestTypes),
    false = exists(?N),
    1 = add(First), true = exists(?N),
    2 = add(Second), true = exists(?N),
    2 = add(Second), true = exists(?N), % add just replace old meta
    3 = add(Third), true = exists(?N),
    F = jmeta_declaration:parse(First),
    S = jmeta_declaration:parse(Second),
    T = jmeta_declaration:parse(Third),
    FK = jmeta_declaration:key(F),
    SK = jmeta_declaration:key(S),
    TK = jmeta_declaration:key(T),
    F = ?MODULE:get(FK),
    S = ?MODULE:get(SK),
    T = ?MODULE:get(TK),
    2 = delete(FK), true = exists(?N),
    1 = delete(SK), true = exists(?N),
    1 = delete(SK), true = exists(?N), % delete by non existing key just ignored
    0 = delete(TK), false = exists(?N).

test_behaviour() ->
    {error, {wrong_namespace, 'some.strange.namespace'}} =
        ?MODULE:get({'some.strange.namespace', 'some.undefined.meta'}),
    {error, {{std, 'some.undefined.meta'}, is_not_defined}} =
        ?MODULE:get({std, 'some.undefined.meta'}).
