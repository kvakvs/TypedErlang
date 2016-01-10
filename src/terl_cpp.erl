%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Dmytro Lytovchenko
%%% @doc Erlang module to create and print/write a C++ source module
%%% @end
%%%-------------------------------------------------------------------
-module(terl_cpp).

%% API
-export([write/2, mod_new/0, mod_funs/2,
    fun_new/4, var_new/2, nested_new/3, call_new/2, literal_new/2,
    var_new/1, var_new/3, nested_simple/1]).

-record(module, {includes=[], funs=[] }).
-record(func, {ret_type, name, args=[], code=[]}).
%% Type can be 'use' which means variable is used here not declared
%% initializer can be undefined which means none, or an expr
-record(var, {name, type, initializer}).

%% Pre-condition nested block: OPERATOR(ARG) { NESTEDCODE; }
-record(nested_pre, {type, arg, code=[]}).
%% Unconditional nested { NESTEDCODE; } to help with scoping
-record(nested_simple, {code=[]}).

-record(call, {name, args=[]}).
%% a literal with orig erlang value; string representation for C++ is selected
%% during generation
-record(lit, {type, value}).

fun_new(RetType, Name, Args, Code) ->
    #func{ret_type=RetType, name=Name, args=Args, code=Code}.

var_new(Name) -> #var{name=Name, type=use}.
var_new(Type, Name) -> #var{name=Name, type=Type}.
var_new(Type, Name, Init) -> #var{name=Name, type=Type, initializer=Init}.

call_new(N, Args) -> #call{name=N, args=Args}.
literal_new(Type, Value) -> #lit{type=Type, value=Value}.

nested_simple(Code) -> #nested_simple{code=Code}.
nested_new(Type, Arg, Code) -> #nested_pre{type=Type, arg=Arg, code=Code}.

%% @doc Format a C++ module to an iolist and dump it to given filename
write(Filename, #module{}=Cpp) ->
    %io:format("~s~n", [format(Cpp)]),
    file:write_file(Filename, format(Cpp)).

mod_new() ->
    I = ["GluonRuntime.h"],
    #module{includes=I}.

format(#module{includes=I, funs=F}) ->
    [format_includes(I, []),
        format_funs(F, [])
    ].

format_includes([], A) -> lists:reverse(A);
format_includes([H|T], A) ->
    A1 = [io_lib:format("#include <\"~s\">~n", [H]) | A],
    format_includes(T, A1).

mod_funs(F, #module{}=M) -> M#module{funs=F}.

format_funs([], A) -> lists:reverse(A);
format_funs([F|T], A) ->
    A1 = [format_fun(F) | A],
    format_funs(T, A1).

format_fun(#func{name=Name, args=Args, ret_type=RetType, code=Code}) ->
    [io_lib:format("~n~s ~s(", [RetType, format_fun_name(Name)]),
        iolist_join(format_args(Args), ", "),
        ") {\n",
        format_code(Code, 1),
        "}\n"
    ].

indent(Level) -> lists:duplicate(Level*4, $ ).

%% @doc Format chunk of code elements and nested elements
format_code(X, IndentLvl) when is_list(X) ->
    [format_code(Y, IndentLvl) || Y <- X];
format_code(#nested_simple{code=NestedCode}, IndentLvl) ->
    %% unconditional nested { NESTEDCODE; }
    OffStr = indent(IndentLvl),
    [OffStr, "do {\n",
        format_code(NestedCode, IndentLvl + 1),
        OffStr, "} while(0);\n"];
format_code(#nested_pre{type=Type, arg=Arg, code=NestedCode}, IndentLvl) ->
    %% Format a C++ block with precondition OPERATOR(ARG) { NESTEDCODE; }
    %% TODO: postcondition OPERATOR { NESTEDCODE; } CHECK(ARG);
    OffStr = indent(IndentLvl),
    [OffStr, io_lib:format("~s (", [Type]),
        format_args(Arg),
        ") {\n",
        format_code(NestedCode, IndentLvl + 1),
        OffStr, "}\n"
        ];
format_code(#call{}=Call, IndentLvl) ->
    [indent(IndentLvl), format_expr(Call)];
format_code(#var{}=A, IndentLvl) ->
    [indent(IndentLvl),
        io_lib:format("~s ~s = ", [A#var.type, A#var.name]),
        format_expr(A#var.initializer),
        ";\n"];
format_code(X, IndentLvl) ->
    [indent(IndentLvl),
        io_lib:format("/* ~p */~n", [X])].

%% @doc Join list of string with another string similar to string.join in python
%% but produce an iolist for efficiency
iolist_join([], _Str) -> [];
iolist_join(List, Str) ->
    [hd(List) | [[Str, Item] || Item <- tl(List)]].

format_args(Args) -> format_expr(Args).

%% @doc Format a value without type, for operation argument lists
%% TODO: Is this same as format_expr???
format_expr(X) when is_list(X) -> [format_expr(Y) || Y <- X];
format_expr(#call{name=N, args=Args}) ->
    [io_lib:format("~s(", [N]),
        iolist_join(format_args(Args), ", "),
        ")"];
format_expr(#lit{type=atom, value=V}) ->
    V1 = string:to_upper(atom_to_list(V)),
    io_lib:format("atom::~s", [V1]);
format_expr(#var{type=use, name=N}) ->
    io_lib:format("~s", [N]);
format_expr(#var{initializer=undefined}=A) ->
    io_lib:format("~s ~s", [A#var.type, A#var.name]);
format_expr(X) -> io_lib:format("/* expr: ~p */", [X]).

format_fun_name({N, _Arity}) ->
    io_lib:format("fun_~s", [N]).
