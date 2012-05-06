-module(walrus).
-author("Devin Torres <devin@devintorres.com>").

-export([compile/1, render/2, render/3]).

-define(is_falsy(V),
    (V =:= false orelse V =:= [] orelse V =:= undefined orelse V =:= null)).

-type value() :: list() | binary() | integer() | float() | atom().
-type context() :: [{Key :: string(), Value :: value()}, ...].
-type stringifiable() :: value() | fun((Context :: context())
    -> value()).

-spec compile(Template :: list() | binary()) ->
    fun((Context :: context(), PartialsContext :: context()) -> binary()).
compile(Template) when is_binary(Template) ->
    compile(binary_to_list(Template));
compile(Template) when is_list(Template) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    fun (Context, PartialsContext) ->
        render(ParseTree, Context, PartialsContext, [])
    end.

-spec render(Template :: list() | binary(), Context :: context())
    -> binary().
render(Template, Context) ->
    render(Template, Context, []).

-spec render(Template :: list() | binary(), Context :: context(), PartialsContext :: context())
    -> binary().
render(Template, Context, PartialsContext) when is_binary(Template) ->
    render(binary_to_list(Template), Context, PartialsContext);
render(Template, Context, PartialsContext) when is_list(Template) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    render(ParseTree, Context, PartialsContext, []).

-spec render(ParseTree :: list(), Context :: context(), PartialsContext :: context(), Acc :: list())
    -> binary().
render([{text, Text} | ParseTree], Context, PartialsContext, Acc) ->
    render(ParseTree, Context, PartialsContext, [Text | Acc]);
render([{var, Key} | ParseTree], Context, PartialsContext, Acc) ->
    Value = get(Key, Context),
    render(ParseTree, Context, PartialsContext, [stringify(Value, Context, true) | Acc]);
render([{var_unescaped, Key} | ParseTree], Context, PartialsContext, Acc) ->
    Value = get(Key, Context),
    render(ParseTree, Context, PartialsContext, [stringify(Value, Context, false) | Acc]);
render([{block, Key, SubParseTree} | ParseTree], Context, PartialsContext, Acc) ->
    Value = get(Key, Context),
    case Value of
        Val when ?is_falsy(Val) ->
            render(ParseTree, Context, PartialsContext, Acc);
        Val when is_list(Val) ->
            Tmpl = [render(SubParseTree, Ctx, PartialsContext, []) || Ctx <- Val],
            render(ParseTree, Context, PartialsContext, [Tmpl | Acc]);
        _ ->
            Tmpl = render(SubParseTree, Context, PartialsContext, []),
            render(ParseTree, Context, PartialsContext, [Tmpl | Acc])
    end;
render([{inverse, Key, SubParseTree} | ParseTree], Context, PartialsContext, Acc) ->
    Value = get(Key, Context),
    case Value of
        Val when ?is_falsy(Val) ->
            Tmpl = render(SubParseTree, Context, PartialsContext, []),
            render(ParseTree, Context, PartialsContext, [Tmpl | Acc]);
        _ ->
            render(ParseTree, Context, PartialsContext, Acc)
    end;
render([], _Context, _PartialsContext, Acc) ->
    iolist_to_binary(lists:reverse(Acc)).

-spec get(Key :: atom(), Context :: context()) -> stringifiable().
get(Key, Context) ->
    Value = proplists:get_value(Key, Context),
    if
        is_function(Value) -> Value(Context);
        true -> Value
    end.

-spec stringify(Value :: stringifiable(), Context :: context(), Escape :: boolean())
    -> iolist().
stringify(Value, _Context, false) when is_list(Value) ->
    Value;
stringify(Value, _Context, true) when is_list(Value) ->
    escape(Value);
stringify(Value, _Context, true) when is_binary(Value) ->
    escape(binary_to_list(Value));
stringify(Value, _Context, _Escape) when is_integer(Value) ->
    integer_to_list(Value);
stringify(Value, _Context, _Escape) when is_float(Value) ->
    walrus_mochinum:digits(Value);
stringify(Value, _Context, false) when is_atom(Value) ->
    atom_to_list(Value);
stringify(Value, _Context, true) when is_atom(Value) ->
    escape(atom_to_list(Value));
stringify(Value, Context, Escape) when is_function(Value) ->
    stringify(Value(Context), Context, Escape).

escape(Value) ->
    escape(Value, []).

-spec escape(Value :: list(), Acc :: list()) -> iolist().
escape([$< | Tail], Acc) ->
    escape(Tail, [<<"&lt;">> | Acc]);
escape([$> | Tail], Acc) ->
    escape(Tail, [<<"&gt;">> | Acc]);
escape([$& | Tail], Acc) ->
    escape(Tail, [<<"&amp;">> | Acc]);
escape([C | Tail], Acc) ->
    escape(Tail, [C | Acc]);
escape([], Acc) ->
    lists:reverse(Acc).
