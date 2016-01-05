%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Dmytro Lytovchenko
%%% @doc Parsing and handling of type representations for TypedErlang needs
%%% @end
%%%-------------------------------------------------------------------
-module(terl_type).

%% API
-export([parse_type/1]).

-define(top_type, term).

-record(t_name, { app=[] :: list() | atom(),
                module=[] :: list() | atom(),
                name=[] :: list() | atom() }).
%% #t_def{name = t_type() | t_var(), type = type()}
-record(t_def, {name, type}). % local definition 'name = type'

%% #t_union{types = [type()]}
-record(t_union, {types = []}). % union type 't1|...|tN'

%% Abstract type name(...)
-record(t_type, {name :: #t_name{},
                args=[] :: list()}).  % :: [type()]

-record(t_fun, {args, range}). % function '(t1,...,tN) -> range'

%% #t_var{name = [] | atom()}
-record(t_var, {name=[]}). % type variable
-record(t_nil, {}).

%% #t_paren{type = type()}
-record(t_paren, {type}). % parentheses

%% #t_list{a = list(), type = type()}
-record(t_list, {type,
                nonempty=false :: boolean()}). % list type '[type]'

%% #t_spec{name = t_name(), type = t_type(), defs = [t_def()]}
-record(t_spec, {name, type, defs=[]}). % function specification

%% #t_integer{ val = integer()}
-record(t_integer_val, {val}). % integer constant

%% #t_integer_range{from = integer(), to = integer()}
-record(t_integer_range, {from, to}).

%% #t_binary{base_size = integer(), unit_size = integer()}
-record(t_binary, {base_size = 0, unit_size = 0}).

-record(t_map, {types=[]}).
-record(t_map_field, {k_type, v_type}).

%% #t_tuple{a = list(), types = [type()]}
-record(t_tuple, {a=[], types = []}). % tuple type '{t1,...,tN}'

%% #t_list{ name = t_atom(), fields = [field()]}
-record(t_record, {name, fields = []}). % record "type" '#r{f1,...,fN}'

%% #t_field{name = type(), type = type()}
-record(t_field, {name, type}). % named field 'n1=t1'

%% #t_atom{ val = atom()}
-record(t_atom, {val}). % atom constant

%%%-------------------------------------------------------------------
parse_type({ann_type,_,[V, T0]}) ->
    %% Note: the -spec/-type syntax allows annotations everywhere, but
    %% EDoc does not. The fact that the annotation is added to the
    %% type here does not necessarily mean that it will be used by the
    %% layout module.
    T = parse_type(T0);
parse_type({remote_type,_,[{atom,_,M},{atom,_,F},Ts0]}) ->
    Ts = parse_type(Ts0),
    #t_type{name = #t_name{module = M, name = F}, args = Ts};
parse_type({type,_,'fun',[{type,_,product,As0},Ran0]}) ->
    [Ran|As] = parse_type([Ran0|As0]),
    #t_fun{args = As, range = Ran};
parse_type({type,_,'fun',[A0={type,_,any},Ran0]}) ->
    [A, Ran] = parse_type([A0, Ran0]),
    #t_fun{args = [A], range = Ran};
parse_type({type,_,'fun',[]}) ->
    #t_type{name = #t_name{name = function}, args = []};
parse_type({type,_,any}) ->
    #t_var{name = '...'}; % Kludge... not a type variable!
parse_type({type,_,nil,[]}) ->
    #t_nil{};
parse_type({paren_type,_,[T]}) ->
    #t_paren{type = parse_type(T)};
parse_type({type,_,list,[T0]}) ->
    T = parse_type(T0),
    #t_list{type = T};
parse_type({type,_,nonempty_list,[T0]}) ->
    T = parse_type(T0),
    #t_list{type = T, nonempty = true};
parse_type({type,_,bounded_fun,[T,Gs]}) ->
    [F0|Defs] = parse_type([T|Gs]),
    %% Assume that the linter has checked type variables.
    #t_spec{type = F0, defs = Defs};
parse_type({type,_,range,[V1,V2]}) ->
    {integer,_,I1} = erl_eval:partial_eval(V1),
    {integer,_,I2} = erl_eval:partial_eval(V2),
    #t_integer_range{from = I1, to = I2};
parse_type({type,_,constraint,[Sub,Ts0]}) ->
    case {Sub,Ts0} of
        {{atom,_,is_subtype},[{var,_,N},T0]} ->
            [T] = parse_type([T0]),
            #t_def{name = #t_var{name = N}, type = T};
        {{atom,_,is_subtype},[ST0,T0]} ->
            %% Should not happen.
            [ST,T] = parse_type([ST0,T0]),
            #t_def{name = ST, type = T};
        _ ->
            erlang:error({element(2, Sub), "cannot handle guard"})
    end;
parse_type({type,_,union,Ts0}) ->
    Ts = parse_type(Ts0),
    #t_union{types = Ts};
parse_type({type,_,tuple,any}) ->
    #t_type{name = #t_name{name = tuple}, args = []};
parse_type({type,_,binary,[Base,Unit]}) ->
    #t_binary{base_size = element(3, Base),
              unit_size = element(3, Unit)};
parse_type({type,_,map,any}) ->
    #t_map{ types = []};
parse_type({type,_,map,Es}) ->
    #t_map{ types = parse_type(Es) };
parse_type({type,_,map_field_assoc,K,V}) ->
    #t_map_field{ k_type = parse_type(K), v_type= parse_type(V) };
parse_type({type,_,map_field_exact,K,V}) ->
    #t_map_field{ k_type = parse_type(K), v_type= parse_type(V) };
parse_type({type,_,tuple,Ts0}) ->
    Ts = parse_type(Ts0),
    #t_tuple{types = Ts};
parse_type({type,_,record,[Name|Fs0]}) ->
    Atom = #t_atom{val = element(3, Name)},
    Fs = parse_type(Fs0),
    #t_record{name = Atom, fields = Fs};
parse_type({type,_,field_type,[Name,Type0]}) ->
    Type = parse_type(Type0),
    #t_field{name = #t_atom{val = element(3, Name)}, type = Type};
parse_type({typed_record_field,{record_field,L,Name},Type}) ->
    parse_type({type,L,field_type,[Name,Type]});
parse_type({typed_record_field,{record_field,L,Name,_E},Type}) ->
    parse_type({type,L,field_type,[Name,Type]});
parse_type({record_field,L,_Name,_E}=F) ->
    parse_type({typed_record_field,F,{type,L,any,[]}}); % Maybe skip...
parse_type({record_field,L,_Name}=F) ->
    parse_type({typed_record_field,F,{type,L,any,[]}}); % Maybe skip...
parse_type({type,_,Name,Types0}) ->
    Types = parse_type(Types0),
    #t_type{name = #t_name{name = Name}, args = Types};
parse_type({var,_,'_'}) ->
    #t_type{name = #t_name{name = ?top_type }};
parse_type({var,_,TypeName}) ->
    #t_var{name = TypeName};
parse_type(L) when is_list(L) ->
    [parse_type(T) || T <- L];
parse_type({atom,_,A}) ->
    #t_atom{val = A};
parse_type(undefined = U) -> % opaque
    U;
parse_type(Expr) ->
    {integer,_,I} = erl_eval:partial_eval(Expr),
    #t_integer_val{val = I}.
