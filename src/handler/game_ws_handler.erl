-module(game_ws_handler).
-export([
         init/2
        ,websocket_init/1
        ,websocket_handle/2
        ,websocket_info/2
        ,terminate/3
    ]).

init(Req, _) ->
    io:format("111111111111111~n", []),
    State = #{ws_pid => none, logined => false, user_pid => none},
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    io:format("222222222222222~n", []),
    WsPid = self(),
    io:format("wwwwwww WsPid: ~p, websocket connected   wwwwwww ~n", [WsPid]),
    NewState = State#{ws_pid := WsPid},
    {ok, NewState}.

websocket_handle({text, <<"@heart">>}, #{ws_pid := WsPid} = State) ->
    io:format("wwwwwww WsPid: ~p, text recevie: ~p   wwwwwww ~n", [WsPid, <<"@heart">>]),
    Resp = integer_to_binary(10000000),
    {reply, {text, Resp}, State};


websocket_handle({text, <<"@stop">>}, #{ws_pid := WsPid} = State) ->
    io:format("wwwwwww @stop: ~p wwwwwwwWsPid: ~p ~n", [<<"@stop">>, WsPid]),
    {stop, State};

websocket_handle({text, Req}, #{ws_pid := WsPid} = State) ->
    io:format("wwwwwww WsPid: ~p, text recevie: ~p wwwwwww ~n", [WsPid, Req]),
    Resp = Req,
    {reply, {text, Resp}, State};

websocket_handle({binary, Req}, #{logined := IsLogined} = State) ->
    <<Cmd:32/little, Bin/binary>> = Req,
    case IsLogined of
        true ->
            UserPid = maps:get(user_pid, State),
            gen_server:cast(UserPid, {cmd_routing, Cmd, Bin}),
            {ok, State};
        false ->
%%            game_debug:debug(error, "xxxxxxxCmd: ~p~n", [Cmd]),
%%            RecordData = pt_10_pb:decode_msg(Bin, pt_10000_c2s),
%%            NewState = pp_account:handler(10000, RecordData, State),
            {ok, State}
    end;

websocket_handle(ping, State) ->
    {reply, {text, <<"pong">>}, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send_binary, Resp}, State) ->
%%    game_debug:debug(error, "binary Data: ~p~n", [Resp]),
    {reply, {binary, Resp}, State};

websocket_info(_Info, #{ws_pid := WsPid} = State) ->
%%    game_debug:debug(info,"Wwwwwwww WsPid: ~p, websocket unkown info  wwwwwww ~n", [WsPid]),
    {reply, {text, <<"unkown info !">>}, State}.

terminate(_Info, _Req, #{ws_pid := WsPid, user_pid := UserPid, logined := IsLogined
        } = _State) ->
        case {IsLogined, is_pid(UserPid)} of
            {true,true} ->
                case is_process_alive(UserPid) of
                    true ->
                        UserPid ! {stop, normal};
                    _ -> notdoing
                end;
            _ ->
                notdoing
        end,
%%    game_debug:debug(error,"wwwwwww WsPid: ~p, websocket terminated   wwwwwww ~n", [WsPid]),
    ok.