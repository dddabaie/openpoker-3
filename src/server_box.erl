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

%%%%开启网关服务
%%start_gateway()->
%%  {ok, Start} = application:get_env(cbtx, gateway_start),
%%  io:format("Gatewany is :~p~n", [Start]),
%%
%%  {ok, Gateway_Server_Port} = application:get_env(cbtx, gateway_port),
%%
%%  if
%%    Start =:= 1 ->
%%      Dispatch = cowboy_router:compile([
%%        {'_', [
%%          {"/", cowboy_static, {priv_file, cbtx, "index.html"}},
%%          {"/post", post_demo, []},
%%          {"/compress/res", compress_response, []},
%%          {"/hello/world", hello_world, []},
%%          {"/eventsource", eventsource_h, []},
%%          {"/static/[...]", cowboy_static, {priv_dir, cbtx, ""}},
%%          {"/rest/basic/auth", rest_basic_auth, []},
%%          {"/hello/to", hello_to, []},
%%          {"/assets/[...]", cowboy_static, {priv_dir, cbtx, "", [{mimetypes, cow_mimetypes, all}]}},
%%
%%          {"/file/server/[...]", cowboy_static, {priv_dir, cbtx, "", [
%%            {mimetypes, cow_mimetypes, all}, {dir_handler, directory_h}]}},
%%
%%          {"/file/upload", cowboy_static, {priv_file, cbtx, "upload.html"}},
%%          {"/do/upload", upload_h, []},
%%          {"/websocket", cowboy_static, {priv_file, cbtx, "ws.html"}},
%%          {"/ws", ws, []},
%%          {"/echo/get", echo_get, []},
%%          {"/echo/get2", echo_get2, []},
%%          {"/echo/post", echo_post, []},
%%          {"/rest/hello/world", rest_hello_world, []},
%%          {"/rest/pastebin/[:paste_id]", rest_pastebin, []},
%%          {"/rest/post/demo", rest_post_demo, []},
%%          {"/get/data/list", query_get, []},
%%          {"/get2/data/list", query_get2, []}
%%        ]}
%%      ]),
%%      {ok, _} = cowboy:start_clear(gateway_listener, [{port, Gateway_Server_Port}], #{
%%        env => #{dispatch => Dispatch},
%%        stream_handlers => [cowboy_compress_h, cowboy_stream_h],
%%        middlewares => [cowboy_router, cowboy_handler]
%%      });
%%    true ->
%%      ok
%%  end.