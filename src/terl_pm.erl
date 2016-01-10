%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, <COMPANY>
%%% @doc Handle pattern matching conversion to C++
%%% @end
%%% Created : 09. jan 2016 23:04
%%%-------------------------------------------------------------------
-module(terl_pm).

%% API
-export([compile/3]).
-include_lib("compiler/src/core_parse.hrl").

%% Parsed pattern match object
%% New_vars - introduced variables and their initializers, created directly
%% Cons - splitting lists
%% Tuples - splitting tuples
%% Comparisons - pairs of expressions which go into if() operator before guard
-record(pmatch, {new_vars, cons, tuples, comparisons}).

%% @doc Takes a list of variables or expressions which go to the left side in PM
%% and list of same length with right variables or expressions, and variable
%% scope. Decides if Lefts introduced new variable or we have to match the
%% rights somehow. Produces a parsed #pmatch{} object which then can be
%% used by caller to produce C++ code about it.
compile(Lefts, Rights, Scope) ->
    {Lefts1, Rights1, NewVars} = find_new_vars(Lefts, Scope, []),
    #pmatch{new_vars = NewVars
    }.
%%    PatsAndArgs = lists:zip(Clause#c_clause.pats, case_args(Case)),
%%    %% If variable was not in scope - create new and assign. Else compare.
%%    VarsCode = [terl_cpp:var_new("const auto" ++ cpp_ref_if_var(Init),
%%        Var#c_var.name,
%%        transpile_expr(Init))
%%        || {Var, Init} <- PatsAndArgs, is_new_variable(Var, Scope)],
%%    Pats0 = [[transpile_expr(Left), transpile_expr(Right)]
%%        || {Right, Left} <- PatsAndArgs, % swap right & left places
%%        not is_new_variable(Left, Scope)],
%%    Pats = lists:flatten(Pats0).


%% @doc Scan left side of pattern match and figure out variables which are new
find_new_vars([], _Scope, A) -> A;
find_new_vars([#c_var{name=Name} | Lefts], Scope, A) ->
    A1 = case sets:is_element(Name, Scope) of
        true -> A; % no changes, it was in scope so not a new variable
        false -> [terl_cpp:var_new("const auto&", Name) | A]
    end,
    find_new_vars(Lefts, Scope, A1).
