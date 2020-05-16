-module(ws).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("openpoker.hrl").

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	erlang:start_timer(1000, self(), <<"Hello!">>),
	Pid = self(),
	lager:info("My Pid is ~p xxxxx", [Pid]),

	{reply, {text, << "##########################">>}, State, hibernate}.
%%	{ok, State}.

websocket_handle({text, Msg}, State) ->
	if
		Msg =:= <<"close">> ->
			self() ! {close, ""};
		true ->
			ok
	end,
	{reply, {text, << "That's what she said! ", Msg/binary >>}, State, hibernate};

websocket_handle({binary, Msg}, State) ->
	handle_data(Msg),
	{replay, {binary, <<Msg>>}};

websocket_handle(_Data, State) ->
	{ok, State, hibernate}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, State, hibernate};

websocket_info({close, _}, State) ->
	self() ! {text, <<"Ready to logout">>},
	{reply, {close, <<"some-reason">>}, State}.

terminate(_Reason, _Req, State)->
	Pid = self(),
	self() ! {text, <<"Ready to logout">>},
	lager:info("pid: ~p is logout", [Pid]),
	lager:info("My Pid is ~p logout", [Pid]),
	{ok, State, hibernate}.

handle_data(Data) when is_list(Data) ->
	Bin = base64:decode(list_to_binary(Data)),
	console([handle_data, Bin]),
	Result = protocol:read(Bin),
	console([handle_protocol, Result]),

	case Result of
		{'EXIT', {Reason, Stack}} ->
			?LOG([{handle_data_error, {Reason, Stack}}]),
			send(#notify_error{error = ?ERR_DATA});
%%			self() ! {close, ""};
		R ->
			send(R)
	end.


console(R) ->
	io:format("===> ~p~n", [R]).

connect() ->
	console(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"),
	put(convert_id_to_process, true).

disconnect(_) ->
	console("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<").

send_data(Data) -> send_data(self(), Data).
send_data(PID, Data) when is_pid(PID), is_binary(Data) ->
	PID ! {send, Data}.

send(R) ->
	D = protocol:write(R),
	Bin = list_to_binary(D),
	Encode = base64:encode(Bin),
	send_data(Encode).
