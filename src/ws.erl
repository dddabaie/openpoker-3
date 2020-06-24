-module(ws).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).
-export([handle_data/2]).

-include("openpoker.hrl").

init(Req, Opts) ->
	io:format("111111111111111~n", []),
	State = #{connection_timer => ?UNDEF, player => ?UNDEF, player_info => ?UNDEF},
	{cowboy_websocket, Req, State, Opts}.

websocket_init(State) ->
	io:format("222222222222222~n", []),
	Pid = self(),
	Timer = erlang:start_timer(?CONNECT_TIMEOUT, Pid, ?MODULE),
	NewState = State#{connection_timer => Timer},
	lager:info("My Pid is ~p ", [Pid]),
	{ok, NewState, hibernate}.

websocket_handle({text, <<"@stop">>}, State) ->
	io:format("ssssssssssssssssstop~n", []),
	{stop, State};

websocket_handle({text, Msg}, State) ->
	if
		Msg =:= <<"close">> ->
			self() ! {close, ""};
		true ->
			ok
	end,
	{reply, {text, << "That's what she said! ", Msg/binary >>}, State, hibernate};

websocket_handle({binary, Msg}, State) ->
	NewState = handle_data(Msg, State),
	{ok, NewState, hibernate};

websocket_handle(_Data, State) ->
	{ok, State, hibernate}.

websocket_info({send, Resp}, State) ->
	{reply, {binary, Resp}, State, hibernate};

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, State, hibernate};

websocket_info({close, _}, State) ->
	self() ! {text, <<"Ready to logout">>},
	{reply, {close, <<"some-reason">>}, State}.

terminate(_Reason, _Req, #{player := Player}=State)->
	disconnect(Player),
	Pid = self(),
	lager:info("pid: ~p is logout", [Pid]),
	{ok, State, hibernate}.

disconnect(Player) when is_pid(Player) ->
	player:phantom(Player),
	ok.

handle_data(Code, LoopData) when is_binary(Code) ->
	case catch protocol:read(base64:decode(Code)) of
		{'EXIT', {_Reason, _Stack}} ->
			error_logger:error_report({protocol, read, Code}),
			send(#notify_error{error = ?ERR_DATA}),
			close(),
			LoopData;
		R ->
			handle_protocol(R, LoopData)
	end;

handle_data(Code, LoopData) when is_list(Code) ->
	handle_data(list_to_binary(Code), LoopData).

%%%%
%%%% handle internal protocol
%%%%

handle_protocol(R = #cmd_login{}, #{connection_timer :=T }=LoopData) when T /= ?UNDEF ->
%%  catch erlang:cancel_connection_timer(T),
	catch erlang:cancel_timer(T),
	NewLoopData = LoopData#{connection_timer := ?UNDEF},
	handle_protocol(R, NewLoopData);

handle_protocol(#cmd_login{identity = Identity, password = Password}, LoopData) ->
	case player:auth(binary_to_list(Identity), binary_to_list(Password)) of
		{ok, unauth} ->
			send(#notify_error{error = ?ERR_UNAUTH}),
			close();
		{ok, player_disable} ->
			send(#notify_error{error = ?ERR_PLAYER_DISABLE}),
			close();
		{ok, pass, Info} ->
			% create player process by client process,
			% receive {'EXIT'} when player process error
			case op_players_sup:start_child(Info) of
				{ok, Player} ->
					player:client(Player),
					player:info(Player),
					player:balance(Player),
					send(#notify_signin{player = Info#tab_player_info.pid}),
					LoopData#{player => Player, player_info => Info};
				{error, already_present} ->
					send(#notify_error{error = ?ERR_PLAYER_BUSY}),
					close();
				{error, {already_started, _Player}} ->
					send(#notify_error{error = ?ERR_PLAYER_BUSY}),
					close()
			end
	end;

handle_protocol(#cmd_logout{}, #{player := Player, player_info := Info} = _LoopData) when is_pid(Player) ->
	op_players_sup:terminate_child(Info#tab_player_info.pid),
	close();

handle_protocol(#cmd_query_game{}, #{player := Player}=LoopData) when is_pid(Player) ->
	GamesList = game:list(),
	lists:map(fun(Info) -> send(Info) end, GamesList),
	send(#notify_games_list_end{size = length(GamesList)}),
	LoopData;

handle_protocol(R,  #{player := Player} = LoopData) when is_pid(Player) ->
	player:cast(Player, R),
	LoopData;

handle_protocol(R, LoopData) ->
	error_logger:warning_report({undef_protocol, R, LoopData}),
	send(#notify_error{error = ?ERR_PROTOCOL}),
	close().

%%send(R) -> send(self(), R).

send(R) ->
	%io:format("~n===============================~n~p~n", [R]),
	case catch protocol:write(R) of
		{'EXIT', Raise} ->
			error_logger:error_report({protocol, write, R, Raise});
		Data ->
			send_data(base64:encode(list_to_binary(Data)))
	end.

send_data(Data) -> send_data(self(), Data).
send_data(PID, Data) when is_pid(PID), is_binary(Data) ->
	PID ! {send, Data}.

close() ->
	self() ! close.





