-module(sender).
-export([start/2]).

start(Host, Port) ->
	{ok,Context} = erlzmq:context(),
	{ok, Socket} = erlzmq:socket(Context, push),
	ok = erlzmq:connect(Socket, "tcp://" ++ Host ++ ":" ++ Port),
	spawn(fun() -> msg_forwarding_loop(Socket) end).

msg_forwarding_loop(Socket) ->
	receive
		{msg, Msg, From} ->
			case erlzmq:send(Socket, Msg) of
				ok -> From ! ok;
				_  -> From ! error
			end,
			msg_forwarding_loop(Socket)
	end.