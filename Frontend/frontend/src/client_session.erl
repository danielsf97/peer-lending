%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, Paradigmas de Sistemas Distribuídos
%%% @doc Modulo representativo da sessão de um cliente autenticado.
%%%
%%% Constitui um loop onde se vão processando mensagens vindas do cliente e 
%%% se entrega à exchange correta, ou provindas do exchange sendo devolvidas ao 
%%% cliente respetivo. 
%%% @author Grupo 3
%%% @end
%%%-----------------------------------------------------------------------------
-module(client_session).
-export([client_loop/2]).
-include("protos_pb.hrl").

%%------------------------------------------------------------------------------
%% @doc Função responsável por servir os pedidos de um cliente.
%%
%% Recebe as mensagens provindas do cliente e entrega-as ao Ator do tipo 
%% "Sender" responsável por comunicar com a exchange respetiva. Retorna as 
%% mensagens que chegam do Ator "Receiver" associado a uma exchange, ao cliente.
%% @end
%%------------------------------------------------------------------------------
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
						{ok, Pid} -> 
							Pid ! {msg, Bin, self()};
						non_existent_company -> 
							Erro = #'ErrorMsg'{error="Empresa não existe!!"},
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
							Erro = #'ErrorMsg'{error="Empresa não existe!!"},
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
						{ok, Pid} -> 
							Pid ! {msg, Bin, self()};
						non_existent_company -> 
							Erro = #'ErrorMsg'{error="Empresa não existe!!"},
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

