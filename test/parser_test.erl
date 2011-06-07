-module(parser_test).

-compile([export_all]).

basic_test() ->
    Tokens = [{text,1,"Hello "},
              {'{{',1},
              {key,1,name},
              {'}}',1},
              {text,1,". Would you like a\n"},
              {'{{#',2},
              {key,2,over18},
              {'}}',2},
              {text,2,"beer"},
              {'{{/',2},
              {key,2,over18},
              {'}}',2},
              {text,2,"\n"},
              {'{{^',3},
              {key,3,over18},
              {'}}',3},
              {text,3,"juice box?"},
              {'{{/',3},
              {key,3,over18},
              {'}}',3},
              {text,3,"?"}],
    Expected = {ok,[{text,"Hello "},
                    {var,name},
                    {text,". Would you like a\n"},
                    {block,over18,[{text,"beer"}]},
                    {text,"\n"},
                    {inverse,over18,[{text,"juice box?"}]},
                    {text,"?"}]},
    Expected = walrus_parser:parse(Tokens).
