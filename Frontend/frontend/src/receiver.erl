%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, Paradigmas de Sistemas Distribuídos
%%% @doc Modulo que cria um ator Receiver associado a uma exchange. 
%%%
%%% Responsável por inicializar um ator cuja função é a de receber mensagens
%%% das exchanges e retorna-las ao ator responsável pela sessão do cliente
%%% respetivo.
%%% @author Grupo 3
%%% @end
%%%-----------------------------------------------------------------------------
-module(receiver).
-export([start/2]).

%%------------------------------------------------------------------------------
%% @doc Função que inicializa um ator Receiver.
%%
%% Faz bind de um socket pull no qual a exchange respetiva deverá se conectar e
%% enviar todas as mensagens destinadas aos clientes.
%% @end
%%------------------------------------------------------------------------------
start(Host, Port) ->
	{ok,Context} = erlzmq:context(),
	{ok, Socket} = erlzmq:socket(Context, [pull, {active, false}]),
	ok = erlzmq:bind(Socket, "tcp://" ++ Host ++ ":" ++ Port),
	spawn(fun() -> msg_forwarding_loop(Socket) end).

%%------------------------------------------------------------------------------
%% @doc Função que contitui o ciclo de vida de um receiver.
%%
%% Processa cada uma das mensagens que lhe chegam da exchange associada,
%% fazendo-as chegar ao ator responsável pela sessão do cliente.
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc Função responsável por enviar uma mensagem assíncrona a uma cliente.
%%
%% Mensagens assíncronas não possuem o PID do ator responsável pela sessão do
%% cliente, pelo que antes de enviar a mensagem tem de contacta o login_manager
%% de forma a conhecer o PID correto.
%% @end
%%------------------------------------------------------------------------------
send_to_client_session(Client, Msg) ->

	case login_manager:get_pid(Client) of

		invalid ->
			pass; %descarta a mensagem

		Pid ->
			Pid ! {receiver, Msg}
		
	end.