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
    terl_cpp:write(Filename, Cpp),
    CoreMod.

transpile(CoreMod) ->
    Cpp0 = terl_cpp:mod_new(),
    Funs = transpile_funs(cerl:module_defs(CoreMod), []),
    terl_cpp:mod_funs(Funs, Cpp0).

transpile_funs([], Accum) -> lists:reverse(Accum);
transpile_funs([Def | T], Accum) ->
    Accum1 = [transpile_fun(Def) | Accum],
    transpile_funs(T, Accum1).

%% @doc Given core function AST produce C++ function block
%% Variable scope for fun is assumed to be empty, and grows deeper as we nest
%% more and more code blocks.
transpile_fun({Name, CoreFun}) ->
    Args = [terl_cpp:var_new("Term", V#c_var.name)
            || V <- CoreFun#c_fun.vars],
    Scope = sets:new(),
    Code = transpile_code(CoreFun#c_fun.body, Scope),
    terl_cpp:fun_new("Term", Name#c_var.name, Args, Code).

%% @doc Given core AST code block produce equal code block in C++
%% Takes core erlang subtree and returns transpiled C++ object from terl_cpp
-spec transpile_code(X :: tuple(), Scope :: sets:set(atom())) -> any().
transpile_code(X, Scope) when is_list(X) ->
    [transpile_code(Elem, Scope) || Elem <- X];
transpile_code(Case=#c_case{}, Scope) -> do_case(Case, Scope);
transpile_code(X, _Scope) -> X.

%% @doc Transpile case clauses with pattern matching arguments
do_case(#c_case{arg=Args, clauses=Clauses}=Case, Scope) ->
    [{'CASE_ARGS', Args}]
    ++ [do_case_clause(Case, Cla, Scope) || Cla <- Clauses].

%%do_case_clause(#c_case{}, Cla=#c_clause{guard=#c_literal{val=true}}, Scope) ->
%%    %% True in a guard removes the guard but keeps the pattern match
%%    %% TODO: pattern match
%%    transpile_code(Cla#c_clause.body, Scope);
do_case_clause(Case=#c_case{}, Clause=#c_clause{}, Scope) ->
    %% TODO: Write proper translation of pattern match for case clause
    %% const auto& X = cor1;
    %% const auto cor4 = erlang::hd(cor0);
    %% const auto cor5 = erlang::tl(cor0);
    %% if (cor5.is_value() && cor4.is_value()) {
    %%    if (helpers::true_check(erlang::op_equal_hard(cor4, X))) {
    io:format("casearg=~p~nclause_pats=~p~nguard=~p~n------~n",
        [case_args(Case), Clause#c_clause.pats, Clause#c_clause.guard]),

%%    PatsAndArgs = lists:zip(Clause#c_clause.pats, case_args(Case)),
%%    %% If variable was not in scope - create new and assign. Else compare.
%%    VarsCode = [terl_cpp:var_new("const auto" ++ cpp_ref_if_var(Init),
%%                                Var#c_var.name,
%%                                transpile_expr(Init))
%%        || {Var, Init} <- PatsAndArgs, is_new_variable(Var, Scope)],
%%    Pats0 = [[transpile_expr(Left), transpile_expr(Right)]
%%                || {Right, Left} <- PatsAndArgs, % swap right & left places
%%                    not is_new_variable(Left, Scope)],
%%    Pats = lists:flatten(Pats0),

    %% TODO: Wrap whole pattern match here with nested do{}while 0
    %% TODO: Here scope will change after new variables are created
    Guard = make_true_check(Clause#c_clause.guard, Clause#c_clause.body, Scope),
%%    %% Nest guard into pattern match block
%%    MatchCode = case Pats of
%%        [] -> Guard;
%%        _ ->
%%            terl_cpp:nested_new("if",
%%                            terl_cpp:call_new("helpers::match_pairs", Pats),
%%                            Guard)
%%    end,
%%    terl_cpp:nested_simple([VarsCode, MatchCode]).

    %% Wrap guard code inside pattern matching code. Guard building function
    %% will recurse deeper in code
    terl_pm:compile(Clause#c_clause.pats, case_args(Case), Scope, Guard).

%% TODO: This can be expression too!
case_args(#c_case{arg=#c_values{}} = Case) ->
    Values = Case#c_case.arg,
    Values#c_values.es;
case_args(#c_case{arg=Arg}) when not is_list(Arg) -> [Arg].

%% @doc Builds a C++ operator 'if' which checks guard value with nested code
make_true_check(Cond, NestedCode, Scope) ->
    terl_cpp:nested_new("if",
        terl_cpp:call_new("helpers::true_check", [transpile_expr(Cond)]),
        transpile_code(NestedCode, Scope)).

%% @doc Given something that looks like expression AST produce equal C++
%% structure
transpile_expr(X) when is_list(X) -> [transpile_expr(Y) || Y <- X];
transpile_expr(#c_call{module=Mod, name= Fun, args=Args}) ->
    call_mfa(Mod, Fun, Args);
transpile_expr(#c_literal{val=Value}) when is_atom(Value) ->
    terl_cpp:literal_new(atom, Value);
transpile_expr(#c_var{name=N}) ->
    terl_cpp:var_new(N);
transpile_expr(X) -> {'EXPR', X}.

%% @doc Given M,F,Args from core AST produce C++ name which will hopefully
%% make sense and exist in support library
call_mfa(#c_literal{val=M}, #c_literal{val=F}, Args) ->
    terl_cpp:call_new(resolve_bif_or_mfa(M, F, length(Args)),
        transpile_expr(Args));
call_mfa(M, F, Args) ->
    terl_cpp:call_new("gluon::apply", [M, F, transpile_expr(Args)]).

%% @doc Given M,F,Arity produce a valid C++ identifier which resolves to a
%% function that we need
resolve_bif_or_mfa(erlang, '==', _Arity) -> "erlang::op_eq_soft";
resolve_bif_or_mfa(erlang, '=:=', _Arity) -> "erlang::op_eq_hard";
resolve_bif_or_mfa(erlang, '>', _Arity) -> "erlang::op_gt";
resolve_bif_or_mfa(erlang, '<', _Arity) -> "erlang::op_lt";
resolve_bif_or_mfa(erlang, '=<', _Arity) -> "erlang::op_leq";
resolve_bif_or_mfa(erlang, '>=', _Arity) -> "erlang::op_geq";
resolve_bif_or_mfa(erlang, '++', _Arity) -> "erlang::op_concat";
resolve_bif_or_mfa(erlang, '+', _Arity) -> "erlang::op_add";
resolve_bif_or_mfa(erlang, '-', _Arity) -> "erlang::op_sub";
resolve_bif_or_mfa(erlang, '*', _Arity) -> "erlang::op_mult";
resolve_bif_or_mfa(erlang, '/', _Arity) -> "erlang::op_float_div";
resolve_bif_or_mfa(M, F, _Arity)        -> io_lib:format("~s::~s", [M, F]).


%% @doc Return true if Var is a Core variable and its name is NOT in Scope
is_new_variable(#c_var{name=N}, Scope) ->
    not sets:is_element(N, Scope);
is_new_variable(_, _Scope) -> false.
