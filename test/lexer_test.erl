-module(lexer_test).

-compile([export_all]).

basic_test() ->
    Tmpl = "Hello {{name}}. Would you like a\n"
           "{{#over18}}beer{{/over18}}\n"
           "{{^over18}}juice box{{/over18}}?",
    Expected = {ok,[{text,1,<<"Hello ">>},
                    {'{{',1},
                    {key,1,name},
                    {'}}',1},
                    {text,1,<<". Would you like a\n">>},
                    {'{{#',2},
                    {key,2,over18},
                    {'}}',2},
                    {text,2,<<"beer">>},
                    {'{{/',2},
                    {key,2,over18},
                    {'}}',2},
                    {text,2,<<"\n">>},
                    {'{{^',3},
                    {key,3,over18},
                    {'}}',3},
                    {text,3,<<"juice box">>},
                    {'{{/',3},
                    {key,3,over18},
                    {'}}',3},
                    {text,3,<<"?">>}],3},
    Expected = walrus_lexer:string(Tmpl).
