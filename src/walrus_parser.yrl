Nonterminals template token var block inverse.

Terminals text key '{{' '{{{' '{{#' '{{/' '{{^' '}}' '}}}'.

Rootsymbol template.

template -> token : ['$1'].
template -> token template : ['$1' | '$2'].

token -> text : {text, value_of('$1')}.
token -> var : '$1'.
token -> block : '$1'.
token -> inverse : '$1'.

var -> '{{' key '}}' : {var, value_of('$2')}.
var -> '{{{' key '}}}' : {var_unescaped, value_of('$2')}.

block -> '{{#' key '}}' template '{{/' key '}}'
       : {block, match_keys('$2', '$6'), '$4'}.

inverse -> '{{^' key '}}' template '{{/' key '}}'
         : {inverse, match_keys('$2', '$6'), '$4'}.

Erlang code.

-compile({inline, line_of/1}).
line_of(Token) ->
    element(2, Token).

-compile({inline, value_of/1}).
value_of(Token) ->
    element(3, Token).

-compile({inline, match_keys/2}).
match_keys(Tok1, Tok2) ->
    K1 = value_of(Tok1),
    K2 = value_of(Tok2),
    if
        K1 =/= K2 ->
            L1 = integer_to_list(line_of(Tok1)),
            L2 = integer_to_list(line_of(Tok2)),
            throw("Missed an opening or closing of a section. "
                ++ atom_to_list(K1) ++ " =/= " ++ atom_to_list(K2)
                ++ " (lines " ++ L1 ++ ", " ++ L2 ++ ")");
        true -> K1
    end.
