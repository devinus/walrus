-module(render_test).

-compile([export_all]).

basic_test() ->
    Template = "Hello {{{name}}}.\n\nDrinks:\n\n{{#drinks}}\n    - {{name}}, {{tastiness}}\n{{/drinks}}",
    Context = [{name, "Devin & Jane"},
               {drinks, [[{name, "Beer"},
                          {tastiness, 5}],
                         [{name, "Juice"},
                          {tastiness, 8}]]}],
    Expected = <<"Hello Devin & Jane.\n\nDrinks:\n\n\n    - Beer, 5\n\n    - Juice, 8\n">>,
    Expected = walrus:render(Template, Context).
