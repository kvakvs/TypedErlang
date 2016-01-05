%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Dmytro Lytovchenko
%%% @doc Parse transform which checks type specs in a file and produces an error
%%%      if they are not sufficient or conflicting. Also use of type any() or
%%%      term() is not allowed.
%%% @end
%%%-------------------------------------------------------------------
-module(typed_erlang).

%% API
-export([parse_transform/2]).

-define(SPEC_TAB, typed_erlang_specs).

parse_transform(ASTIn, _Options) ->
    init(),
    walk_ast(ASTIn, []).

init() ->
    ets:new(?SPEC_TAB, [named_table]).

walk_ast([], ASTOut) -> lists:reverse(ASTOut);
walk_ast([E | RestASTIn], ASTOut) ->
    io:format("AST: ~p~n", [E]),
    walk_ast(RestASTIn, [handle_ast(E) | ASTOut]).

handle_ast({'attribute', _Line0, 'spec', Spec} = Attr) ->
    {MFA, [Spec0]} = Spec,
    {'type', _Line1, _FunOrBoundedFun, Spec1} = Spec0,
    Type = terl_type:parse_type(Spec1),
    io:format("Spec: MFA=~p ~p~n", [MFA, Type]),
    ets:insert(?SPEC_TAB, {MFA, Type}),
    Attr;
handle_ast(X) -> X.
