%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Dmytro Lytovchenko
%%% @doc Erlang module to create and print/write a C++ source module
%%% @end
%%%-------------------------------------------------------------------
-module(terl_cppmod).

%% API
-export([write/2, mod_new/0, mod_funs/2,
    fun_new/4, var_new/2, nested/3, call/2]).

-record(cppmod, {includes=[],
                funs=[]
                }).

-record(cppfun, {ret_type,
                name,
                args=[],
                code=[]}).

-record(cppvar, {name,
                type
                }).

-record(nested, {type, arg, code=[]}).
-record(call, {name, args=[]}).

call(N, Args) -> #call{name=N, args=Args}.

nested(Type, Arg, Ops) ->
    #nested{type=Type, arg=Arg, code=Ops}.

write(Filename, #cppmod{}=Cpp) ->
    %io:format("~s~n", [format(Cpp)]),
    file:write_file(Filename, format(Cpp)).

mod_new() ->
    I = ["GluonRuntime.h"],
    #cppmod{includes=I}.

format(#cppmod{includes=I, funs=F}) ->
    [format_includes(I, []),
        format_funs(F, [])
    ].

format_includes([], A) -> lists:reverse(A);
format_includes([H|T], A) ->
    A1 = [io_lib:format("#include <\"~s\">~n", [H]) | A],
    format_includes(T, A1).

mod_funs(F, #cppmod{}=M) -> M#cppmod{funs=F}.

format_funs([], A) -> lists:reverse(A);
format_funs([F|T], A) ->
    A1 = [format_fun(F) | A],
    format_funs(T, A1).

format_fun(#cppfun{name=Name, args=Args, ret_type=RetType, code=Code}) ->
    [io_lib:format("~n~s ~s(", [RetType, format_fun_name(Name)]),
        iolist_join(format_args(Args), ", "),
        ") {\n",
        format_code(Code),
        "}\n"
    ].

format_code(X) when is_list(X) -> [format_code(Y) || Y <- X];
format_code(#nested{type=Type, arg=Arg, code=Code}) ->
    [io_lib:format("~s(", [Type]),
        format_args(Arg),
        ") {\n",
        format_code(Code),
        "}\n"
        ];
format_code(#call{}=Call) -> format_expr(Call);
format_code(X) -> io_lib:format("/* ~p */~n", [X]).

iolist_join([], _Str) -> [];
iolist_join(List, Str) ->
    [hd(List) | [[Str, Item] || Item <- tl(List)]].

format_args(Args) -> format_arg(Args).

format_arg(X) when is_list(X) -> [format_arg(Y) || Y <- X];
format_arg(#call{}=C) -> format_expr(C);
format_arg(#cppvar{}=A) ->
    io_lib:format("~s ~s", [A#cppvar.type, A#cppvar.name]);
format_arg(X) -> io_lib:format("/* arg= ~p */", [X]).

format_fun_name({N, Arity}) ->
    io_lib:format("fun_~s", [N]).

fun_new(RetType, Name, Args, Code) ->
    #cppfun{ret_type=RetType, name=Name, args=Args, code=Code}.

var_new(Type, Name) ->
    #cppvar{name=Name, type=Type}.

format_expr(#call{name=N, args=Args}) ->
    [io_lib:format("~s(", [N]),
        format_args(Args),
        ");\n"].
