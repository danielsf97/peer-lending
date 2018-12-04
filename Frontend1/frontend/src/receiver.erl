-module(receiver).
-export([start/2]).

start(Host, Port) ->
	{ok,Context} = erlzmq:context(),
	{ok, Socket} = erlzmq:socket(Context, pull),
	ok = erlzmq:connect(Socket, "tcp://" ++ Host ++ ":" ++ Port).
% 	spawn(fun() -> msg_forwarding_loop(Socket) end).

% msg_forwarding_loop(Socket) ->
% 	case erlzmq:recv(Socket) of
% 		{ok, Bin} ->

