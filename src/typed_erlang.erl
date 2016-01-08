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
transpile_fun({Name, CoreFun}) ->
    Args = [terl_cpp:var_new("Term", V#c_var.name)
            || V <- CoreFun#c_fun.vars],
    Code = transpile_code(CoreFun#c_fun.body),
    terl_cpp:fun_new("Term", Name#c_var.name, Args, Code).

%% @doc Given core AST code block produce equal code block in C++
transpile_code(X) when is_list(X) ->
    [transpile_code(Elem) || Elem <- X];
transpile_code(Case=#c_case{}) ->
    do_case(Case);
transpile_code(X) -> X.

%% @doc Transpile case clauses with pattern matching arguments
do_case(#c_case{arg=Args, clauses=Clauses}=Case) ->
    [{case_args, Args}]
    ++ [do_case_clause(Case, Cla) || Cla <- Clauses].

do_case_clause(#c_case{}, Cla=#c_clause{guard=#c_literal{val=true}}) ->
    transpile_code(Cla#c_clause.body);
do_case_clause(#c_case{}, Cla=#c_clause{}) ->
    %% TODO: Write proper translation of pattern match for case clause
    %% const auto& X = cor1;
    %% const auto cor4 = erlang::hd(cor0);
    %% const auto cor5 = erlang::tl(cor0);
    %% if (cor5.is_value() && cor4.is_value()) {
    %%    if (helpers::true_check(erlang::op_equal_hard(cor4, X))) {

    Guard = terl_cpp:nested_new("if",
        terl_cpp:call_new("helpers::true_check",
            [transpile_expr(Cla#c_clause.guard)]),
        transpile_code(Cla#c_clause.body)),
    %% Nest guard into pattern match block
    terl_cpp:nested_new("if",
        terl_cpp:call_new("helpers::match_pairs",
                        transpile_expr(Cla#c_clause.pats)),
        Guard).

%% @doc Given something that looks like expression AST produce equal C++
%% structure
transpile_expr(X) when is_list(X) -> [transpile_expr(Y) || Y <- X];
transpile_expr(#c_call{module=Mod, name= Fun, args=Args}) ->
    call_mfa(Mod, Fun, Args);
transpile_expr(#c_literal{val=Value}) when is_atom(Value) ->
    terl_cpp:literal_new(atom, Value);
transpile_expr(#c_var{name=N}) ->
    terl_cpp:var_new(N);
transpile_expr(X) -> {expr, X}.

%% @doc Given M,F,Args from core AST produce C++ name which will hopefully
%% make sense and exist in support library
call_mfa(#c_literal{val=M}, #c_literal{val=F}, Args) ->
    terl_cpp:call_new(resolve_bif_or_mfa(M, F, length(Args)),
        transpile_expr(Args));
call_mfa(M, F, Args) ->
    terl_cpp:call_new("gluon::apply", [M, F, transpile_expr(Args)]).

%% @doc Given M,F,Arity produce a valid C++ identifier which resolves to a
%% function that we need
resolve_bif_or_mfa(erlang, '==', _Arity) -> "erlang::op_equal_soft";
resolve_bif_or_mfa(erlang, '=:=', _Arity) -> "erlang::op_equal_hard";
resolve_bif_or_mfa(erlang, '>', _Arity) -> "erlang::op_greater";
resolve_bif_or_mfa(erlang, '<', _Arity) -> "erlang::op_less";
resolve_bif_or_mfa(erlang, '=<', _Arity) -> "erlang::op_less_equal";
resolve_bif_or_mfa(erlang, '>=', _Arity) -> "erlang::op_greater_equal";
resolve_bif_or_mfa(erlang, '++', _Arity) -> "erlang::op_concat";
resolve_bif_or_mfa(erlang, '+', _Arity) -> "erlang::op_add";
resolve_bif_or_mfa(erlang, '-', _Arity) -> "erlang::op_subtract";
resolve_bif_or_mfa(erlang, '*', _Arity) -> "erlang::op_multiply";
resolve_bif_or_mfa(erlang, '/', _Arity) -> "erlang::op_float_div";
resolve_bif_or_mfa(M, F, _Arity)        -> io_lib:format("~s::~s", [M, F]).