-module(receiver).
-export([start/2]).

start(Host, Port) ->
	{ok,Context} = erlzmq:context(),
	{ok, Socket} = erlzmq:socket(Context, [ pull, {active, true} ]),
	ok = erlzmq:bind(Socket, "tcp://" ++ Host ++ ":" ++ Port),
	spawn(fun() -> msg_forwarding_loop(Socket) end).

msg_forwarding_loop(Socket) ->
	receive
		{ok, Bin} ->
			{'MessageWrapper', Msg} = protos_pb:decode_msg(Bin, 'MessageWrapper'),
			{MsgType, Body} = Msg,

			case MsgType of

				companyactionresp ->
					{'CompanyActionResp', Client_B, _} = Body,
					Client = binary_to_list(Client_B),
					send_to_client_session(Client, Bin);

				investoractionresp ->
					{'InvestorActionResp', Client_B, _} = Body,
					Client = binary_to_list(Client_B),
					send_to_client_session(Client, Bin)

			end,

			msg_forwarding_loop(Socket)
	end.

send_to_client_session(Client, Msg) ->

	login_manager:get_pid(Client),

	receive
		{login_manager, Pid} ->
			Pid ! {receiver, Msg};

		{login_manager, invalid} ->
			pass %descarta a mensagem
			
	end.