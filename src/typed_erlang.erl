%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Dmytro Lytovchenko
%%% @doc Parse transform which checks type specs in a file and produces an error
%%%      if they are not sufficient or conflicting. Also use of type any() or
%%%      term() is not allowed.
%%% @end
%%%-------------------------------------------------------------------
-module(typed_erlang).

%% API
-export([parse_transform/2, core_transform/2]).

-define(SPEC_TAB, typed_erlang_specs).
-include_lib("compiler/src/core_parse.hrl").

parse_transform(ASTIn, _Options) ->
    init(),
    walk_ast(ASTIn, []).

init() ->
    ets:new(?SPEC_TAB, [named_table]).

walk_ast([], ASTOut) -> lists:reverse(ASTOut);
walk_ast([E | RestASTIn], ASTOut) ->
    %io:format("AST: ~p~n", [E]),
    walk_ast(RestASTIn, [handle_ast(E) | ASTOut]).

handle_ast({'attribute', _Line0, 'spec', Spec} = Attr) ->
    {MFA, [Spec0]} = Spec,
    {'type', _Line1, _FunOrBoundedFun, Spec1} = Spec0,
    Type = terl_type:parse_type(Spec1),
    %io:format("Spec: MFA=~p ~p~n", [MFA, Type]),
    ets:insert(?SPEC_TAB, {MFA, Type}),
    Attr;
handle_ast(X) -> X.

-spec core_transform(cerl:c_module(), _) -> cerl:c_module().
core_transform(CoreMod, _Opts) ->
    %io:format("Defs ~p~n", [cerl:module_defs(Mod)]),
    %io:format("Exps ~p~n", [cerl:module_exports(Mod)]),
    Cpp = transpile(CoreMod),
    Modname = cerl:module_name(CoreMod),
    Filename = atom_to_list(Modname#c_literal.val) ++ ".cpp",
    terl_cppmod:write(Filename, Cpp),
    CoreMod.

transpile(CoreMod) ->
    Cpp0 = terl_cppmod:mod_new(),
    Funs = transpile_funs(cerl:module_defs(CoreMod), []),
    terl_cppmod:mod_funs(Funs, Cpp0).

transpile_funs([], Accum) -> lists:reverse(Accum);
transpile_funs([Def | T], Accum) ->
    Accum1 = [transpile_fun(Def) | Accum],
    transpile_funs(T, Accum1).

transpile_fun({Name, CoreFun}) ->
    Args = [terl_cppmod:var_new("Term", V#c_var.name)
            || V <- CoreFun#c_fun.vars],
    Code = transpile_code(CoreFun#c_fun.body),
    terl_cppmod:fun_new("Term", Name#c_var.name, Args, Code).

transpile_code(X) when is_list(X) ->
    [transpile_code(Elem) || Elem <- X];
transpile_code(#c_case{arg=Args, clauses=Clauses}) ->
    [{case_args, Args}]
    ++ [terl_cppmod:nested("if",
            transpile_expr(Cla#c_clause.guard),
            transpile_code(Cla#c_clause.body)) || Cla <- Clauses];
transpile_code(X) -> X.

transpile_expr(X) when is_list(X) -> [transpile_expr(Y) || Y <- X];
transpile_expr(#c_call{module=Mod, name= Fun, args=Args}) ->
    call_mfa(Mod, Fun, Args);
transpile_expr(X) -> {expr, X}.

call_mfa(#c_literal{val=M}, #c_literal{val=F}, Args) ->
    terl_cppmod:call(io_lib:format("~s::~s", [M, F]), Args);
call_mfa(M, F, Args) ->
    terl_cppmod:call("gluon::apply", [M, F, Args]).
