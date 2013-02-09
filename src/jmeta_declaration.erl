%% Author: Said
%% Created: 08.02.2013
%% Description: TODO: Add description to jmeta_declaration
-module(jmeta_declaration).

%%
%% Include files
%%

-include("jmeta.hrl").

%%
%% Exported Functions
%%

% test
-export([test/0]).

% api
-export([new_type/1,
         new_frame/1]).

%%
%% API Functions
%%

test() ->
    test_type(),
    test_frame(),
    {ok, done}.

new_type({type, Name, Meta}) when is_atom(Name) ->
    case jframe:new(Meta) of
        {error, wrong_frame} -> {error, meta_wrong_frame};
        _ ->
            {Mixins, Constraints, Default, Mode} =
                jframe:find([{mixins, []},
                             {constraints, []},
                             default,
                             {mode, [{mixins, all},
                                     {constraints, all}]}], Meta),
            case Mixins =:= [] andalso Constraints =:= [] of
                true -> {error, meta_is_empty};
                false ->
                    case lists:all(fun is_atom/1, Mixins) of
                        false -> {error, mixins_must_be_atoms};
                        true ->
                            case lists:all(fun(Fun) -> {arity, 1} =:= erlang:fun_info(Fun, arity) end, Constraints) of
                                false -> {error, constraints_must_be_unary_funs};
                                true ->
                                    case jframe:new(Mode) of
                                        {error, wrong_frame} -> {error, mode_wrong_frame};
                                        _ ->
                                            {MixinsMode, ConstraintsMode} =
                                                jframe:find([{mixins, all},
                                                             {constraints, all}], Mode),
                                            AllOrAny = fun(all) -> true; (any) -> true; (_) -> false end,
                                            case lists:all(AllOrAny, [MixinsMode, ConstraintsMode]) of
                                                false -> {error, mode_wrong_arguments};
                                                true ->
                                                    #type{name=Name,
                                                          mixins=jtils:list_distinct(Mixins),
                                                          constraints=Constraints,
                                                          default = Default,
                                                          mode=#tmode{mixins=MixinsMode,
                                                                      constraints=ConstraintsMode}}
                                            end
                                    end
                            end
                    end
            end
    end;
new_type(_) -> {error, wrong_type_format}.

new_frame({frame, _Name, _Meta}) ->
    ok;
new_frame(_) -> {error, wrong_frame_format}.

%%
%% Local Functions
%%

test_type() ->
    {error, wrong_type_format} = new_type(1),
    {error, wrong_type_format} = new_type({1, 1, 1}),
    {error, wrong_type_format} = new_type({type, 1, 1}),
    {error, meta_wrong_frame} = new_type({type, int, 1}),
    {error, meta_is_empty} = new_type({type, int, []}),
    {error, meta_is_empty} = new_type({type, int, [{mixins, []}]}),
    {error, meta_is_empty} = new_type({type, int, [{constraints, []}]}),
    {error, meta_is_empty} = new_type({type, int, [{mixins, []}, {constraints, []}]}),
    {error, mixins_must_be_atoms} = new_type({type, int, [{mixins, [1, 2, 3]}]}),
    {error, constraints_must_be_unary_funs} = new_type({type, int, [{constraints, [fun(1, 1) -> true end]}]}),
    {error, mode_wrong_arguments} = new_type({type, int, [{constraints, [fun is_integer/1]}, {mode, [{mixins, abc}]}]}),
    {error, mode_wrong_arguments} = new_type({type, int, [{constraints, [fun is_integer/1]}, {mode, [{constraints, abc}]}]}),
    T1 = #type{name=int,
               mixins=[number],
               constraints=[],
               default=undefined,
               mode=#tmode{constraints=all,
                           mixins=all}} = new_type({type, int, [{mixins, [number, number]}]}),
    T2 = T1#type{default=0},
    T2 = new_type({type, int, [{mixins, [number]}, {default, 0}]}),
    T3 = T2#type{mode=T2#type.mode#tmode{constraints=any}},
    T3 = new_type({type, int, [{mixins, [number]}, {default, 0}, {mode, [{constraints, any}]}]}),
    IsInteger = fun is_integer/1,
    T4 = T3#type{constraints=[IsInteger]},
    T4 = new_type({type, int, [{mixins, [number]},
                               {constraints, [IsInteger]},
                               {default, 0}, {mode, [{constraints, any}]}]}).

test_frame() ->
    ok.
