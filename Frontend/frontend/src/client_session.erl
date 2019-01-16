-module(client_session).
-export([client_loop/2]).
-include("protos_pb.hrl").

client_loop(Sock, User) ->
	receive
		{tcp, Sock, Bin} ->
			{'MessageWrapper', _, _, Msg} = protos_pb:decode_msg(Bin, 'MessageWrapper'),
			{MsgType, Body} = Msg,

			case MsgType of

				logoutreq ->
					case login_manager:logout(User) of
						ok -> 
							LogoutResp = #'LogoutResp'{status='SUCCESS'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{msgType = 'SYNC', inner_message = {logoutresp, LogoutResp}}),
							gen_tcp:send(Sock, Resp);
						invalid ->
							LogoutResp = #'LogoutResp'{status='ERROR'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{msgType = 'SYNC', inner_message = {logoutresp, LogoutResp}}),
							gen_tcp:send(Sock, Resp),
							client_loop(Sock, User)
					end;

				emissionfixedratereq ->

					{'EmissionFixedRateReq', Comp_B} = Body,
					Comp = binary_to_list(Comp_B),
					senders_manager ! {company, Comp, self()},

					receive
						{ok, Pid} -> Pid ! {msg, Bin, self()};
						non_existent_company -> 
							Erro = #'ErrorMsg'{error='Empresa não existe!!'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{msgType = 'SYNC', inner_message = {errormsg, Erro}}),
							gen_tcp:send(Sock, Resp)
					end,
					client_loop(Sock, User);

				companyactionreq ->
					{'CompanyActionReq', Comp_B, _, _, _} = Body,
					Comp = binary_to_list(Comp_B),
					senders_manager ! {company, Comp, self()},

					receive
						{ok, Pid} -> Pid ! {msg, Bin, self()};
						non_existent_company -> 
							Erro = #'ErrorMsg'{error='Empresa não existe!!'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{msgType = 'SYNC', inner_message = {errormsg, Erro}}),
							gen_tcp:send(Sock, Resp)
					end,
					client_loop(Sock, User);

				investoractionreq ->
					case Body of
						{'InvestorActionReq', _, Comp_B, _, _, _} ->
							Comp = binary_to_list(Comp_B),
							senders_manager ! {company, Comp, self()};
						{'InvestorActionReq', _, Comp_B, _, _} ->
							Comp = binary_to_list(Comp_B),
							senders_manager ! {company, Comp, self()}
					end,
					receive
						{ok, Pid} -> Pid ! {msg, Bin, self()};
						non_existent_company -> 
							Erro = #'ErrorMsg'{error='Empresa não existe!!'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{msgType = 'SYNC', inner_message = {errormsg, Erro}}),
							gen_tcp:send(Sock, Resp)
					end,
					client_loop(Sock, User)

			end;

		{receiver, Resp} ->
			gen_tcp:send(Sock, Resp),
			client_loop(Sock, User);

		{tcp_closed, _} ->
			login_manager:logout(User)
	end.

