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
compile_([L | Lefts], [R | Rights], Scope, _NestedCode, A) ->
    A1 = match(L, R, Scope, A),
    compile(Lefts, Rights, Scope, A1).
%% when matching left cons or tuple, check each variable if it is free, if not
%% - compile to a comparison else to a new variable.

%% @doc Guess the action required for the pair L,R in Scope, and append to Acc
match(#c_var{name=LName} = L, R, Scope, Acc) ->
    case sets:is_element(LName, Scope) of
        true ->
            Acc ++ [comparison(L, R)]; % left is a new free variable
        false ->
            Acc ++ [terl_cpp:var_new("const auto&", L#c_var.name)]
            %% TODO: Scope is changed here, update scope
    end.

%% @doc Decide either additional condition for if or a separate if block to
%% compare values L and R
comparison(L, R) ->
    erlang:error(not_implemented).
