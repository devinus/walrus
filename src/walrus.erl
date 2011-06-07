-module(walrus).

-export([render/2]).

-define(is_falsy(V),
    (V =:= undefined orelse V =:= null orelse V =:= false orelse V =:= [])).

render(Template, Context) ->
    {ok, Tokens, _} = walrus_lexer:string(Template),
    {ok, ParseTree} = walrus_parser:parse(Tokens),
    render(ParseTree, Context, []).

render([{text, Text} | ParseTree], Context, Acc) ->
    render(ParseTree, Context, [Text | Acc]);
render([{var, Var} | ParseTree], Context, Acc) ->
    Val = proplists:get_value(Var, Context),
    render(ParseTree, Context, [Val | Acc]);
render([{block, Key, SubParseTree} | ParseTree], Context, Acc) ->
    Val = proplists:get_value(Key, Context),
    case Val of
        V when ?is_falsy(V) ->
            render(ParseTree, Context, Acc);
        Ctx ->
            Tmpl = render(SubParseTree, Ctx, []),
            render(ParseTree, Context, [Tmpl | Acc])
    end;
render([{inverse, Key, SubParseTree} | ParseTree], Context, Acc) ->
    Val = proplists:get_value(Key, Context),
    case Val of
        V when ?is_falsy(V) ->
            Tmpl = render(SubParseTree, Context, []),
            render(ParseTree, Context, [Tmpl | Acc]);
        _ ->
            render(ParseTree, Context, Acc)
    end;
render([], _Context, Acc) ->
    lists:flatten(lists:reverse(Acc)).
