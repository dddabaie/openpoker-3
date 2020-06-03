%%%-------------------------------------------------------------------
%%% @author lunay
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 2月 2020 2:18 下午
%%%-------------------------------------------------------------------
-module(server_box).
-author("lunay").

%% API
-export([start_game_server/0
%%  ,start_gateway/0
]).

start_game_server()->
  %% TCP Websocket
  GameServerDispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, openpoker, "ws.html"}},
      {"/ws", ws, []}
    ]}
  ]),

%%	提取websocket/server_port服务端口
  {ok, Websocket_Port} = application:get_env(openpoker, websocket_port),
  {ok, Server_Port} = application:get_env(openpoker, server_port),

  {ok, _} = cowboy:start_clear(game_server_listener, [{port, Websocket_Port}], #{
    env => #{dispatch => GameServerDispatch},
    stream_handlers => [cowboy_compress_h, cowboy_stream_h],
    middlewares => [cowboy_router, cowboy_handler]
  }),

  {ok, _} = ranch:start_listener(tcp_reverse,
    ranch_tcp, [{port, Server_Port}], reverse_protocol, []).
