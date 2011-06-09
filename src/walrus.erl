-module(walrus).
-author("Devin Torres <devin@devintorres.com>").

-export([compile/1, render/2]).

-define(is_falsy(V),
    (V =:= false orelse V =:= [] orelse V =:= undefined orelse V =:= null)).

compile(Template) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    fun (Context) ->
        render(ParseTree, Context, [])
    end.

render(Template, Context) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    render(ParseTree, Context, []).

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

get(Key, Context) ->
    Value = proplists:get_value(Key, Context),
    if
        is_function(Value) -> Value(Context);
        true -> Value
    end.

stringify(Value, _Context, _Escape) when is_integer(Value) ->
    integer_to_list(Value);
stringify(Value, _Context, _Escape) when is_float(Value) ->
    mochinum:digits(Value);
stringify(Value, Context, Escape) when is_function(Value) ->
    stringify(Value(Context), Context, Escape);
stringify(Value, _Context, true) when is_atom(Value) ->
    escape(atom_to_list(Value));
stringify(Value, _Context, false) when is_atom(Value) ->
    atom_to_list(Value);
stringify(Value, _Context, true) ->
    escape(Value);
stringify(Value, _Context, false) ->
    Value.

escape(Value) ->
    escape(Value, []).

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
