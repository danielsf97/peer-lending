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
			{'MessageWrapper', _, Msg} = protos_pb:decode_msg(Bin, 'MessageWrapper'),
			{MsgType, Body} = Msg,

			case MsgType of

				emissionfixedrateresp ->
					{'EmissionFixedRateResp', Client_B, _} = Body,
					Client = binary_to_list(Client_B),
					send_to_client_session(Client, Bin);

				companyactionresp ->
					{'CompanyActionResp', Client_B, _} = Body,
					Client = binary_to_list(Client_B),
					send_to_client_session(Client, Bin);

				investoractionresp ->
					{'InvestorActionResp', Client_B, _} = Body,
					Client = binary_to_list(Client_B),
					send_to_client_session(Client, Bin);

				auctionemissionresult ->
					{'AuctionEmissionResult', Client_B, _} = Body,
					Client = binary_to_list(Client_B),
					send_to_client_session(Client, Bin)

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