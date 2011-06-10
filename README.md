Walrus - Mustache-like Templating
=================================

Walrus is 50% Mustache and 50% large flippered marine mammal.

Lexing is done with Leex and parsing is done with Yecc, both
of which are included in newer versions of Erlang.

Most Mustache constructs work, such as variables, unescaped
variables, blocks, and inverse blocks. However, The Walrus is
an opinionated animal, and partials will never be supported.
Also, functions passed into the context are simple evaluated
and are not passed the raw template.

There is no real "compilation" step, however you can skip
lexing and parsing templates every time you want to render
them by using `compile/1`, which returns a lambda you can
then pass a context that renders the template.

Examples
--------

### Simple `render`

    1> Tmpl = "Hello {{{name}}}.
    1> 
    1> Drinks:
    1> 
    1> {{#drinks}}
    1>     - {{name}}, {{tastiness}}
    1> {{/drinks}}".
    "Hello {{{name}}}.\n\nDrinks:\n\n{{#drinks}}\n    - {{name}}, {{tastiness}}\n{{/drinks}}"
    2> 
    2> Ctx = [{name, "Devin & Jane"},
    2>        {drinks, [[{name, "Beer"},
    2>                   {tastiness, 5}],
    2>                  [{name, "Juice"},
    2>                   {tastiness, 8}]]}].
    [{name,"Devin & Jane"},
     {drinks,[[{name,"Beer"},{tastiness,5}],
              [{name,"Juice"},{tastiness,8}]]}]
    3> 
    3> walrus:render(Tmpl, Ctx).
    <<"Hello Devin & Jane.\n\nDrinks:\n\n\n    - Beer, 5\n\n    - Juice, 8\n">>

### "Compiled" `fun`

    1> Tmpl = walrus:compile("Hello {{{name}}}.\n\nDrinks:\n\n{{#drinks}}\n    - {{name}}, {{tastiness}}\n{{/drinks}}").
    #Fun<walrus.0.90102984>
    2> Ctx = [{name, "Devin & Jane"},
    2>        {drinks, [[{name, "Beer"},
    2>                   {tastiness, 5}],
    2>                  [{name, "Juice"},
    2>                   {tastiness, 8}]]}].
    [{name,"Devin & Jane"},
     {drinks,[[{name,"Beer"},{tastiness,5}],
              [{name,"Juice"},{tastiness,8}]]}]
    3> 
    3> Tmpl(Ctx).
    <<"Hello Devin & Jane.\n\nDrinks:\n\n\n    - Beer, 5\n\n    - Juice, 8\n">>

Acknowledgments
---------------
Robert Virding helped me tremendously when I struggled to
understand Leex. He's also responsible for
`([^{}]|({[^{])|(}[^}]))+`.

License
-------

All code released into the public domain (see `UNLICENSE`)
except for the file `mochinum.erl`, which has it's own
license (see `LICENSE`).
