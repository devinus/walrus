-module(walrus).
-author("Devin Torres <devin@devintorres.com>").

-export([compile/1, render/2]).

-define(is_falsy(V),
    (V =:= false orelse V =:= [] orelse V =:= undefined orelse V =:= null)).

-type value() :: list() | binary() | integer() | float() | atom().
-type context() :: [{Key :: string(), Value :: value()}, ...].
-type stringifiable() :: value() | fun((Context :: context())
    -> value()).

-spec compile(Template :: list() | binary()) -> fun((Context :: context())
    -> binary()).
compile(Template) when is_binary(Template) ->
    compile(binary_to_list(Template));
compile(Template) when is_list(Template) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    fun (Context) ->
        render(ParseTree, Context, [])
    end.

-spec render(Template :: list() | binary(), Context :: context())
    -> binary().
render(Template, Context) when is_binary(Template) ->
    render(binary_to_list(Template), Context);
render(Template, Context) when is_list(Template) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    render(ParseTree, Context, []).

-spec render(ParseTree :: list(), Context :: context(), Acc :: list())
    -> binary().
render([{text, Text} | ParseTree], Context, Acc) ->
    render(ParseTree, Context, [Text | Acc]);
render([{var, Key} | ParseTree], Context, Acc) ->
    Value = get(Key, Context),
    render(ParseTree, Context, [stringify(Value, Context, true) | Acc]);
render([{var_unescaped, Key} | ParseTree], Context, Acc) ->
    Value = get(Key, Context),
    render(ParseTree, Context, [stringify(Value, Context, false) | Acc]);
render([{block, Key, SubParseTree} | ParseTree], Context, Acc) ->
    Value = get(Key, Context),
    case Value of
        Val when ?is_falsy(Val) ->
            render(ParseTree, Context, Acc);
        Val when is_list(Val) ->
            Tmpl = [render(SubParseTree, Ctx, []) || Ctx <- Val],
            render(ParseTree, Context, [Tmpl | Acc]);
        _ ->
            Tmpl = render(SubParseTree, Context, []),
            render(ParseTree, Context, [Tmpl | Acc])
    end;
render([{inverse, Key, SubParseTree} | ParseTree], Context, Acc) ->
    Value = get(Key, Context),
    case Value of
        Val when ?is_falsy(Val) ->
            Tmpl = render(SubParseTree, Context, []),
            render(ParseTree, Context, [Tmpl | Acc]);
        _ ->
            render(ParseTree, Context, Acc)
    end;
render([], _Context, Acc) ->
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
stringify(Value, _Context, false) when is_binary(Value) ->
    binary_to_list(Value);
stringify(Value, _Context, true) when is_binary(Value) ->
    escape(binary_to_list(Value));
stringify(Value, _Context, false) when is_list(Value) ->
    Value;
stringify(Value, _Context, true) when is_list(Value) ->
    escape(Value);
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
