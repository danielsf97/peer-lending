-module(receiver).
-export([start/2]).

start(Host, Port) ->
	{ok,Context} = erlzmq:context(),
	{ok, Socket} = erlzmq:socket(Context, [pull, {active, false}]),
	ok = erlzmq:bind(Socket, "tcp://" ++ Host ++ ":" ++ Port),
	spawn(fun() -> msg_forwarding_loop(Socket) end).

msg_forwarding_loop(Socket) ->
	case erlzmq:recv(Socket) of
		{ok, Bin} ->
			case protos_pb:decode_msg(Bin, 'MessageWrapper') of
				{'MessageWrapper', _, undefined, Msg} ->
					{auctionemissionresult, Body} = Msg,
					{'AuctionEmissionResult', Client_B, _} = Body,
					Client = binary_to_list(Client_B),
					send_to_client_session(Client, Bin);
				{'MessageWrapper', _, BinToken, _} ->
					Token = binary_to_list(BinToken),
					ClientSessionPid = list_to_pid(Token),
					ClientSessionPid ! {receiver, Bin}
			end,

			msg_forwarding_loop(Socket)
	end.

send_to_client_session(Client, Msg) ->

	case login_manager:get_pid(Client) of

		invalid ->
			pass; %descarta a mensagem

		Pid ->
			Pid ! {receiver, Msg}
		
	end.