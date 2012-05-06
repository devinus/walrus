-module(compile_test).

-compile([export_all]).

basic_test() ->
    Template = "Hello {{{name}}}.\n"
               "Drinks:\n"
               "{{#drinks}}\n"
               "    - {{name}}, {{tastiness}}\n"
               "{{/drinks}}",

    Renderer = walrus:compile(Template),

    Context = [{name, "Devin & Jane"},
               {drinks, [[{name, "Beer"},
                          {tastiness, 5}],
                         [{name, "Juice"},
                          {tastiness, 8}]]}],

    Expected = <<"Hello Devin & Jane.\n"
                 "Drinks:\n\n"
                 "    - Beer, 5\n\n"
                 "    - Juice, 8\n">>,

    Expected = Renderer(Context, []).
