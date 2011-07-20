-module(env).

-ifdef(TEST).
%% -compile(export_all).
-export([setup/0, teardown/0]).
-export([read/1, comp/2]).

-include_lib("eunit/include/eunit.hrl").

-define(FIXTURES, "fixtures").
-define(EXPECTS, "expects").

%% @doc Setup enviroment and start apps 
setup()->
    ok.

teardown()->
    ok.

read(Fixture) ->
    {ok, Bin} = file:read_file(filename:join(?FIXTURES, Fixture)),
    Bin.

comp(Result, Expect) ->
    {ok, ExBin} = file:read_file(filename:join(?EXPECTS, Expect)),
    ?assertEqual(Result, ExBin),
    ok.

-endif.