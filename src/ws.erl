-module(ws).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).
-export([encode_data/1]).

-include("openpoker.hrl").

-record(pdata, {
	connection_timer = ?UNDEF,
	player = ?UNDEF,
	player_info = ?UNDEF
}).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	Pid = self(),
	lager:info("Client Pid is ~p xxxxx", [Pid]),
	State = connect(),
	{ok, State}.

websocket_handle({text, Msg}, State) ->
	if
		Msg =:= <<"close">> ->
			self() ! {close, ""};
		true ->
			ok
	end,
	{reply, {text, << "That's what she said! ", Msg/binary >>}, State, hibernate};

websocket_handle({binary, Msg}, State) ->
	handle_data(Msg, State),
	{ok, State};

websocket_handle(_Data, State) ->
	{ok, State, hibernate}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, State, hibernate};

websocket_info({close, _}, State) ->
	self() ! {text, <<"Ready to logout">>},
	{reply, {close, <<"some-reason">>}, State};

websocket_info(close, State) ->
	{reply, {close, <<"close">>, State}}.

terminate(_Reason, _Req, State)->
	Pid = self(),
	self() ! {text, <<"Ready to logout">>},
	lager:info("pid: ~p is logout", [Pid]),
	lager:info("My Pid is ~p logout", [Pid]),
	{ok, State, hibernate}.


connect() ->
	connect(?CONNECT_TIMEOUT).

connect(ConnectTimeout) ->
	Timer = erlang:start_timer(ConnectTimeout, self(), ?MODULE),
	#pdata{connection_timer = Timer}.

disconnect(#pdata{player = Player}) when is_pid(Player) ->
	player:phantom(Player),
	ok;

disconnect(_) ->
	ok.

close() ->
	self() ! close.

handle_message({timeout, _, ?MODULE}, LoopData = #pdata{connection_timer =T}) when T =:= ?UNDEF ->
	LoopData;

handle_message({timeout, _, ?MODULE}, _LoopData) ->
	send(#notify_error{error = ?ERR_CONNECTION_TIMEOUT}),
	close().

handle_data(Code, LoopData) when is_binary(Code) ->
	case catch protocol:read(base64:decode(Code)) of
		{'EXIT', {_Reason, _Stack}} ->
			error_logger:error_report({protocol, read, Code}),
			send(#notify_error{error = ?ERR_DATA}),
			close();
		R ->
			handle_protocol(R, LoopData)
	end;

handle_data(Code, LoopData) when is_list(Code) ->
	handle_data(list_to_binary(Code), LoopData).

%%%%
%%%% handle internal protocol
%%%%

handle_protocol(R = #cmd_login{}, LoopData = #pdata{connection_timer =T}) when T /= ?UNDEF ->
  catch erlang:cancel_timer(T),
	handle_protocol(R, LoopData#pdata{connection_timer = ?UNDEF});

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
					LoopData#pdata{player = Player, player_info = Info};
				{error, already_present} ->
					send(#notify_error{error = ?ERR_PLAYER_BUSY}),
					close();
				{error, {already_started, _Player}} ->
					send(#notify_error{error = ?ERR_PLAYER_BUSY}),
					close()
			end
	end;

handle_protocol(#cmd_logout{}, #pdata{player = Player, player_info = Info}) when is_pid(Player) ->
	op_players_sup:terminate_child(Info#tab_player_info.pid),
	close();

handle_protocol(#cmd_query_game{}, LoopData = #pdata{player = Player}) when is_pid(Player) ->
	GamesList = game:list(),
	lists:map(fun(Info) -> send(Info) end, GamesList),
	send(#notify_games_list_end{size = length(GamesList)}),
	LoopData;

handle_protocol(R, LoopData = #pdata{player = Player}) when is_pid(Player) ->
	player:cast(Player, R),
	LoopData;

handle_protocol(R, LoopData) ->
	error_logger:warning_report({undef_protocol, R, LoopData}),
	send(#notify_error{error = ?ERR_PROTOCOL}),
	close().

send(R) -> encode_data(R).

encode_data(R) ->
	io:format("~n===============================~n~p~n", [R]),
	case catch protocol:write(R) of
		{'EXIT', Raise} ->
			error_logger:error_report({protocol, write, R, Raise}),
			{error, <<>>};
		Data ->
			{ok, base64:encode(list_to_binary(Data))}
	end.



%%handle_data(Data) when is_list(Data) ->
%%	Bin = base64:decode(list_to_binary(Data)),
%%	console([handle_data, Bin]),
%%	Result = protocol:read(Bin),
%%	console([handle_protocol, Result]),
%%
%%	case Result of
%%		{'EXIT', {Reason, Stack}} ->
%%			?LOG([{handle_data_error, {Reason, Stack}}]),
%%			send(#notify_error{error = ?ERR_DATA});
%%%%			self() ! {close, ""};
%%		R ->
%%			send(R)
%%	end.
%%
%%
%%send_data(Data) -> send_data(self(), Data).
%%send_data(PID, Data) when is_pid(PID), is_binary(Data) ->
%%	PID ! {send, Data}.
%%
%%send(R) ->
%%	D = protocol:write(R),
%%	Bin = list_to_binary(D),
%%	Encode = base64:encode(Bin),
%%	send_data(Encode).
