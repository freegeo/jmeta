%%% -------------------------------------------------------------------
%%% Author  : Said
%%% Description :
%%%
%%% Created : 11.02.2013
%%% -------------------------------------------------------------------
-module(jmeta_type_cache).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("jmeta.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, add/1, delete/1, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    case R = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, _} -> lists:foreach(fun add/1, jmeta_library:types()), R;
        _ -> R
    end.

add(Meta) ->
    case jmeta_declaration:parse_type(Meta) of
        {error, _} = E -> E;
        Type -> gen_server:cast(?MODULE, {add, Type}), ok
    end.

delete(Name) ->
    gen_server:cast(?MODULE, {delete, Name}), ok.

get(Name) ->
    gen_server:call(?MODULE, {get, Name}).

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
handle_call({get, Name}, _, State) ->
    Result =
        case dict:find(Name, State) of
            {ok, Value} -> Value;
            error -> {error, type_is_not_defined}
        end,
    {reply, Result, State};
handle_call(_, _, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({add, Type}, State) ->
    {noreply, dict:store(Type#type.name, Type, State)};
handle_cast({delete, Name}, State) ->
    {noreply, dict:erase(Name, State)};
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

