%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, Paradigmas de Sistemas Distribuídos
%%% @doc Modulo que cria um ator Sender associado a uma exchange. 
%%%
%%% Responsável por inicializar um ator cuja função é a de perante cada mensagem
%%% que chegue vinda do ator responsável pela sessão do cliente, a encaminhar
%%% para a exchange a que se encontra associado.
%%% @author Grupo 3
%%% @end
%%%-----------------------------------------------------------------------------
-module(sender).
-export([start/2]).

%%------------------------------------------------------------------------------
%% @doc Função que inicializa um ator Sender.
%%
%% Faz connect ao socket pull da exchange respetiva e para o qual deverá enviar
%% todas as mensagens. Inicializa o ciclo de vida do ator.
%% @end
%%------------------------------------------------------------------------------
start(Host, Port) ->
	{ok,Context} = erlzmq:context(),
	{ok, Socket} = erlzmq:socket(Context, push),
	ok = erlzmq:connect(Socket, "tcp://" ++ Host ++ ":" ++ Port),
	spawn(fun() -> msg_forwarding_loop(Socket) end).

%%------------------------------------------------------------------------------
%% @doc Função que contitui o ciclo de vida de um Sender.
%%
%% Reencaminha cada uma das mensagens vindas dos Atores Sessão dos clientes,
%% para a exchange associada.
%% @end
%%------------------------------------------------------------------------------
msg_forwarding_loop(Socket) ->
	receive
		{msg, Msg, From} ->
			case erlzmq:send(Socket, Msg) of
				ok -> From ! ok;
				_  -> From ! error
			end,
			msg_forwarding_loop(Socket)
	end.