-module(tupls).

-compile([{core_transform, typed_erlang}]).

-export([f/1]).

f({A, B, _}) -> A + B.