Definitions.

Key = [a-zA-Z0-9_]+

Rules.

{{!.*}} : skip_token.
([^{}]|({[^{])|(}[^}]))+ : {token,{text,TokenLine,?ltb(TokenChars)}}.
{{   : {token,{'{{',TokenLine}}.
{{#  : {token,{'{{#',TokenLine}}.
{{/  : {token,{'{{/',TokenLine}}.
{{\^ : {token,{'{{^',TokenLine}}.
{{{  : {token,{'{{{',TokenLine}}.
{{>  : {token,{'{{>',TokenLine}}.
\s*{Key}\s*}} : {token,{key,TokenLine,?key(TokenChars,TokenLen)},"}}"}.
}}   : {token,{'}}',TokenLine}}.
}}}  : {token,{'}}}',TokenLine}}.

Erlang code.

-define(ltb(List), list_to_binary(List)).

-define(key(Chars, Len),
    list_to_atom(string:strip(string:left(Chars, Len-2)))).
