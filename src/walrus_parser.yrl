Nonterminals template token var block inverse.

Terminals text key '{{' '{{{' '{{#' '{{/' '{{^' '}}' '}}}'.

Rootsymbol template.

template -> token : ['$1'].
template -> token template : ['$1' | '$2'].

token -> text : {text, ?value_of('$1')}.
token -> var : '$1'.
token -> block : '$1'.
token -> inverse : '$1'.

var -> '{{' key '}}' : {var, ?value_of('$2')}.
var -> '{{{' key '}}}' : {var_unescaped, ?value_of('$2')}.

block -> '{{#' key '}}' template '{{/' key '}}'
       : {block, ?keys_match(?value_of('$2'), ?value_of('$6')), '$4'}.

inverse -> '{{^' key '}}' template '{{/' key '}}'
         : {inverse, ?keys_match(?value_of('$2'), ?value_of('$6')), '$4'}.

Erlang code.

-define(value_of(Token), (element(3, Token))).
-define(keys_match(K1, K2),
    (if K1 =/= K2 -> throw("keys don't match"); true -> K1 end)).
