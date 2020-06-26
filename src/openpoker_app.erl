-module(openpoker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  application:start(lager),
  application:ensure_started(ranch),
  application:ensure_started(cowlib),
  application:ensure_started(cowboy),
  case mnesia:system_info(tables) of
    [schema] ->
      io:format("==============================================~n"),
      io:format(" REBUILD CORE ...~n"),
      schema:rebuild_schema(),
      schema:rebuild_core_and_data(),
      io:format("REBUILD CORE SUCCESSFUL~n");
    _ ->
      ok
  end,
  io:format(">>>>>>>>>>>>>>>>>>>>>>>>>>~n"),

  server_box:start_game_server(),
  op_sup:start_link().


stop(_State) ->
  server_box:stop(),
  application:stop(gun),
  io:format("<<<<<<<<<<<<<<<<<<<<<<<<<~n"),
  ok.
