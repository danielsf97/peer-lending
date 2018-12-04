-module(client_session).
-export([client_loop/1]).
-include("protos_pb.hrl").

client_loop(Sock) ->
	receive
		{tcp, Sock, Bin} ->
			{'MessageWrapper', Msg} = protos_pb:decode_msg(Bin, 'MessageWrapper'),
			{MsgType, Body} = Msg,
			case MsgType of

				logoutreq ->
					{'LogoutReq', User_B} = Body,
					User = binary_to_list(User_B),
					case login_manager:logout(User) of
						ok -> 
							LogoutResp = #'LogoutResp'{status='SUCCESS'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {logoutresp, LogoutResp}}),
							gen_tcp:send(Sock, Resp);
						invalid ->
							LogoutResp = #'LogoutResp'{status='ERROR'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {logoutresp, LogoutResp}}),
							gen_tcp:send(Sock, Resp),
							client_loop(Sock)
					end;

				companyactionreq ->
					case Body of
						{'CompanyActionReq', Comp_B, _, _, _} ->
							Comp = binary_to_list(Comp_B),
							senders_manager ! {company, Comp, self()};
						{'CompanyActionReq', Comp_B, _, _} ->
							Comp = binary_to_list(Comp_B),
							senders_manager ! {company, Comp, self()}
					end,
					receive
						{ok, Pid} -> Pid ! {msg, Bin, self()}, io:fwrite("Mensagem Enviada ~n",[]);
						non_existent_company -> 
							Erro = #'ErrorMsg'{error='Empresa não existe!!'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {errormsg, Erro}}),
							gen_tcp:send(Sock, Resp)
					end;

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
						{ok, Pid} -> Pid ! {msg, Bin, self()}, io:fwrite("Mensagem Enviada ~n",[]);
						non_existent_company -> 
							Erro = #'ErrorMsg'{error='Empresa não existe!!'},
							Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {errormsg, Erro}}),
							gen_tcp:send(Sock, Resp)
					end
						

			end
	end.

