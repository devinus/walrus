-module(partials_test).

-compile([export_all]).

basic_test() ->
    HelloTemplate = "Hello {{{name}}}.\n"
                    "Drinks:",
    Template = "{{> hello }}\n"
               "{{#drinks}}\n"
               "    - {{name}}, {{tastiness}}\n"
               "{{/drinks}}",
    Context = [{name, "Devin & Jane"},
               {drinks, [[{name, "Beer"},
                          {tastiness, 5}],
                         [{name, "Juice"},
                          {tastiness, 8}]]}],
    Expected = <<"Hello Devin & Jane.\n"
                 "Drinks:\n\n"
                 "    - Beer, 5\n\n"
                 "    - Juice, 8\n">>,
    Expected = walrus:render(Template, Context, [{hello, HelloTemplate}]).

nested_test() ->
    MainTemplate = "My name is {{name}}\n"
                   "I plan to travel into following places:\n"
                   
                   "{{#places}}\n  {{> place_item }}\n{{/places}}\n",
    
    PlaceItemTemplate = "{{name}} ({{country}}): {{> place_description }}",
    PlaceDescTemplate = "{{description}}",
    Context = [{name, "Vladimir"},
               {places, [
                 [{name, "San Jose"}, {country, "USA"}, {description, "Very interesting place"}],
                 [{name, "Kiev"}, {country, "Ukraine"}, {description, "Great place"}]
    ]}],
    PartialsContext = [{place_item, PlaceItemTemplate}, {place_description, PlaceDescTemplate}],
    Expected = <<"My name is Vladimir\n"
                 "I plan to travel into following places:\n\n"
                 "  San Jose (USA): Very interesting place\n\n"
                 "  Kiev (Ukraine): Great place\n\n">>,
    Expected = walrus:render(MainTemplate, Context, PartialsContext).
    