%---- Cowboy Gun Websocket Implementation Without Behaviours ----
-module(gun_ex).
-compile([export_all,nowarn_export_all]).
-include("openpoker.hrl").
-export([
  wc/0,
  connection/1
]).

%%websocket 客户端测试
wc()->
  Pid = spawn(gun_ex,connection,[#{host => "127.0.0.1",port => 3009,path => "/ws"}]),
  Pid ! start,
  Pid.

connection(State) ->
  receive
    start ->
      #{host := Host,port := Port} = State,
      io:format("host: ~p port: ~p~n", [Host, Port]),
      %------- Connect To Host ----------
      {ok,WPID} = gun:open(Host,Port),
      io:format("Opening Gun: ~p~n",[WPID]),
      connection(State#{wpid => WPID});
    {gun_up,WPID,_Proto} ->
      #{path := Path} = State,
      io:format("Gun Up: ~p~n",[WPID]),
      %------- Upgrade Connection To WebSocket -------
      gun:ws_upgrade(WPID,Path,[],#{compress => true}),
      io:format("Upgrading.~n",[]),
      connection(State#{socket => WPID});
%%      connection(State);
    {gun_down,WPID,ws,closed,_,_} ->
      %------ Connection Closed ------
      io:format("Gun Down:~p~n",[WPID]),
      connection(State);
    {gun_ws_upgrade,WPID,ok,_Data} ->
      %------ Connection Upgraded to websocket ---
      io:format("Upgraded:~p~n",[WPID]),
      connection(State#{socket => WPID});
    {gun_response, WPID, _Ref, Code, HttpStatusCode, Headers} -> %-- Code : fin/nofin
      %------ Error Response While Upgrade To WebSocket -------
      io:format("Response:~p|~p|~p~n",[Code,HttpStatusCode,Headers]),
      gun:flush(WPID),
      connection(State);
  %------ Error In Child Connection Process ----
    {gun_error, WPID, Ref, Reason} ->
      gun:flush(WPID),
      io:format("Gun Error :~p|~p|~p~n",[WPID,Ref, Reason]),
      connection(State);
  %------ Child Connection Process Went Down -------
    {'DOWN',PID,process,WPID,Reason} ->
      io:format("Gun DOWN:~p|~p|~p~n",[PID,WPID,Reason]),
      gun:flush(WPID),
      gun:shutdown(WPID),
      connection(State);
  %------- Frame Received On WebSocket ------
    {gun_ws, _WPID, _, Frame} ->
      case Frame of
        close ->
          self() ! stop,
          io:format("Received Close Frame W/O Payload~n",[]);
        {close,Code,Message} ->
          self() ! stop,
          io:format("Received Close Frame With Payload: ~p|~p~n",[Code,Message]);
        {text,TextData} ->
          io:format("Received Text Frame: ~p~n",[TextData]);
        {binary, BinData} ->
          <<Cmd:32/little, Bin/binary>> = BinData,
          [H1, H2, _, _, _] = integer_to_list(Cmd),
          %%处理器模块
          ProtocolModule = list_to_atom("pt_"++[H1, H2]++"_pb"),
          RecordName = list_to_atom("pt_"++integer_to_list(Cmd) ++ "_s2c"),
          RecordData = ProtocolModule:decode_msg(Bin, RecordName),
          io:format("Received RecordData: ~p Cmd: ~p~n",[RecordData, Cmd]),
          io:format("Received Binary Frame: ~p~n",[BinData]);
        _ ->
          io:format("Received Unhandled Frame: ~p~n",[Frame])
      end,
      connection(State);
  %-------- GUN EVENTS DONE ------
  %-------- Events From Balancer ------
    cstop ->
      #{socket := Socket} = State,
      gun:ws_send(Socket, {text, <<"@stop">>}),
      connection(State);
    stop ->
      #{wpid := WPID} = State,
      gun:flush(WPID),
      gun:shutdown(WPID);
    login ->
      #{socket := Socket} = State,
      R = #cmd_login{identity = <<"jack">>, password = <<"def_pwd">>},
      Bin = base64:encode(list_to_binary(protocol:write(R))),
      gun:ws_send(Socket, {binary, Bin});

%%    pt_10000 ->
%%      Cmd = 10000,
%%      RecordData = #pt_10000_c2s{user_id = 1},
%%      ProtocolModule = list_to_atom("pt_10_pb"),
%%      io:format("Module: ~p~n", [ProtocolModule]),
%%      Bin = ProtocolModule:encode_msg(RecordData),
%%      BinRecordData = <<Cmd:32/little, Bin/binary>>,
%%      #{socket := Socket} = State,
%%      gun:ws_send(Socket, {binary, BinRecordData}),
%%      connection(State);
%%    pt_10001 ->
%%      Cmd = 10001,
%%      RecordData = #pt_10001_c2s{},
%%      ProtocolModule = list_to_atom("pt_10_pb"),
%%      io:format("Module: ~p~n", [ProtocolModule]),
%%      Bin = ProtocolModule:encode_msg(RecordData),
%%      BinRecordData2 = <<Cmd:32/little, Bin/binary>>,
%%      #{socket := Socket} = State,
%%      gun:ws_send(Socket, {binary, BinRecordData2}),
%%      connection(State);
    Message ->
      io:format("Received Unknown Message on Gun: ~p~n",[Message]),
      connection(State)
  after 30 * 1000 ->
    Socket = maps:get(socket, State, notfound),
    io:format("xxxxx State: ~p ~n", [State]),
    case Socket of
      notfound ->
        ok;
      _ ->
        gun:ws_send(Socket, {text, "game server client~n"})
    end,
    connection(State)
  end.