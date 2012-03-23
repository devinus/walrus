-module(tpl_bench).
-author("Ivan Blinkov <ivan@blinkov.ru>").

-export([run/0]).
%% Mustache.erl: https://github.com/mojombo/mustache.erl
%% Walrus: https://github.com/devinus/walrus
%% 
%% Sample results:
%%
%% Mustache ---
%%  Total time: 5.2954s
%%  Mean render time: 0.5295ms
%% Walrus ---
%%  Total time: 0.0209s
%%  Mean render time: 0.0021ms

-define(COUNT, 10000).

run() ->
  Beer = dict:from_list([{name, "Beer"}, {tastiness, 5}]),
  Juice = dict:from_list([{name, "Juice"}, {tastiness, 8}]),
  Ctx1 = dict:from_list([{customer, "Devin & Jane"},{drinks,[Beer,Juice]}]),
  Ctx2 = [{customer, "Devin & Jane"}, {drinks, [[{name, "Beer"}, {tastiness, 5}], [{name, "Juice"}, {tastiness, 8}]]}],
  Template = "Hello {{{customer}}}.\n\nDrinks:\n\n{{#drinks}}\n    - {{name}}, {{tastiness}}\n{{/drinks}}",
  CT1 = mustache:compile(Template),
  CT2 = walrus:compile(Template),
  T0 = erlang:now(),
  render(CT1, Ctx1, ?COUNT),
  T1 = erlang:now(),
  Diff1 = timer:now_diff(T1, T0),
  Mean1 = Diff1 / ?COUNT,
  io:format("Mustache ---"),
  io:format("~nTotal time: ~.4fs~n", [Diff1 / 1000000]),
  io:format("Mean render time: ~.4fms~n", [Mean1 / 1000]),

  T2 = erlang:now(),
  render(CT2, Ctx2, ?COUNT),
  T3 = erlang:now(),
  Diff2 = timer:now_diff(T3, T2),
  Mean2 = Diff2 / ?COUNT,

  io:format("Walrus ---"),
  io:format("~n Total time: ~.4fs~n", [Diff2 / 1000000]),
  io:format(" Mean render time: ~.4fms~n", [Mean2 / 1000]).

render(CT, Ctx, 1) ->
  Out = CT(Ctx),
  %% uncomment to show one rendering result
  % io:format(Out,[]),
  ok;
render(CT, Ctx, N) ->
  CT(Ctx),
  render(CT, Ctx, N - 1).
