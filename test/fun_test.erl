-module(fun_test).

-compile(export_all).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

expect_test_() -> 
    {"Fun test",
     setup, fun env:setup/0,
     fun(_) -> env:teardown() end,  
     [
     {"Just test env", 
      fun() ->
              env:comp(env:read("basic.html"), "basic.html")
      end},
     {"Just test basic.html", 
      fun() ->
              Result = walrus:render(env:read("basic.html"), []),
              env:comp(Result, "basic.html")
      end},
     {"Just test bigutf.html", 
      fun() ->
              Result = walrus:render(env:read("big-utf8.html"), []),
              env:comp(Result, "big-utf8.html"),
              ok
      end}
    ]}.

-endif.