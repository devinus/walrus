Definitions.

K = [a-zA-Z0-9_]+

Rules.

{{!.*}} : skip_token.
([^{}]|({[^{])|(}[^}]))+ : {token,{text,TokenLine,TokenChars}}.
{{   : {token,{'{{',TokenLine}}.
{{#  : {token,{'{{#',TokenLine}}.
{{/  : {token,{'{{/',TokenLine}}.
{{\^ : {token,{'{{^',TokenLine}}.
{{{  : {token,{'{{{',TokenLine}}.
\s*{K}\s*}} : {token,{key,TokenLine,?to_key(TokenChars,TokenLen)},"}}"}.
}}   : {token,{'}}',TokenLine}}.
}}}  : {token,{'}}}',TokenLine}}.

Erlang code.

-define(to_key(TokenChars, TokenLen),
    (list_to_atom(string:strip(string:left(TokenChars, TokenLen-2))))).
