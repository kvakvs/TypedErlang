%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Dmytro Lytovchenko
%%% @doc Handle pattern matching conversion to C++
%%% @end
%%% Created : 09. jan 2016 23:04
%%%-------------------------------------------------------------------
-module(terl_pm).

%% API
-export([compile/4]).
-include_lib("compiler/src/core_parse.hrl").

%% @doc Takes a list of variables or expressions which go to the left side in PM
%% and list of same length with right variables or expressions, and variable
%% scope. Produces a list of C++ definitions for newly introduced variables (not
%% in scope) and match checks. Nestedcode goes inside the innermost if() {...}
compile(Lefts, Rights, Scope, NestedCode) ->
    compile_(Lefts, Rights, Scope, NestedCode, []).

compile_([], [], _Scope, _Nested, A) -> lists:reverse(A);
compile_([L=#c_var{} | Lefts], [R | Rights], Scope, _NestedCode, A) ->
    A1 = case sets:is_element(L#c_var.name, Scope) of
             true -> comparison(L, R); % left is a new free variable
             false -> [terl_cpp:var_new("const auto&", L#c_var.name) | A]
         end,
    compile(Lefts, Rights, Scope, A1).
%% when matching left cons or tuple, check each variable if it is free, if not
%% - compile to a comparison else to a new variable.
