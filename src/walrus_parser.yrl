Nonterminals template token var partial block inverse.

Terminals text key '{{' '{{{' '{{#' '{{>' '{{/' '{{^' '}}' '}}}'.

Rootsymbol template.

template -> token : ['$1'].
template -> token template : ['$1' | '$2'].

token -> text : {text, ?value('$1')}.
token -> var : '$1'.
token -> block : '$1'.
token -> inverse : '$1'.
token -> partial : '$1'.

var -> '{{' key '}}' : {var, ?value('$2')}.
var -> '{{{' key '}}}' : {var_unescaped, ?value('$2')}.

partial -> '{{>' key '}}' : {partial, ?value('$2')}.

block -> '{{#' key '}}' template '{{/' key '}}'
    : section(block, '$2', '$6', '$4').

inverse -> '{{^' key '}}' template '{{/' key '}}'
    : section(inverse, '$2', '$6', '$4').

Erlang code.

-define(value(Token), element(3, Token)).

-type token() :: {atom(), integer(), atom() | list()}.

-spec section(Type :: atom(), token(), token(), list())
    -> {Type :: atom(), Key1 :: atom(), Tmpl :: list()}.
section(Type, {_, _, Key1}, {_, _, Key2}, Tmpl) when Key1 =:= Key2 ->
    {Type, Key1, Tmpl};
section(_, {_, Line, Key}, _, _) ->
    return_error(Line, {"Unmatched section tag", Key}).
