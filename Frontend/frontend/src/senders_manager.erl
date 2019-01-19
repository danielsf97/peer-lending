%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, Paradigmas de Sistemas Distribuídos
%%% @doc Modulo que cria o Ator que mapeia empresas a Senders. 
%%%
%%% Responsável por inicializar um ator cuja função é criar cada um dos Senders
%%% associados às mais diversas empresas e por responder aos pedidos de 
%%% mapeamento empresa-sender.
%%% @author Grupo 3
%%% @end
%%%-----------------------------------------------------------------------------
-module(senders_manager).
-export([start/0]).

%%------------------------------------------------------------------------------
%% @doc Função que inicializa o ator senders_manager e o regista no sistema.
%%
%% Cria os senders e inicializa o ciclo de vida do ator senders_manger.
%% @end
%%------------------------------------------------------------------------------
start() ->
	Senders = create_senders(),
	CompSendersMap = get_comp_senders_mapping(),
	register(?MODULE, spawn ( fun() -> manage_senders(Senders, CompSendersMap) end) ).

%%------------------------------------------------------------------------------
%% @doc Função que cria os diversos Senders.
%%
%% Faz connect ao socket pull da exchange respetiva e para o qual deverá enviar
%% todas as mensagens. Inicializa o ciclo de vida do ator.
%% @end
%%------------------------------------------------------------------------------
create_senders() ->
	Pid_sender1 = sender:start("localhost", "1234"),
	Pid_sender2 = sender:start("localhost", "1235"),
	Pid_sender3 = sender:start("localhost", "1236"),
	SenderMap = #{
		sender1 => Pid_sender1,
		sender2 => Pid_sender2,
		sender3 => Pid_sender3
	},
	SenderMap.

%%------------------------------------------------------------------------------
%% @doc Função que cria o Mapa de associação de empresas a Senders.
%% @end
%%------------------------------------------------------------------------------
get_comp_senders_mapping() ->
	CompSendersMap = #{
		"empA" => sender1,
		"empB" => sender1,
		"empC" => sender2,
		"empD" => sender3
	},
	CompSendersMap.

%%------------------------------------------------------------------------------
%% @doc Função representativa do ciclo de vida do ator senders_manager.
%%
%% Responsável por responder a cada um dos pedidos que lhe chegam de mapeamento
%% empresa com o Sender associado à Exchange que gere a mesma.
%% @end
%%------------------------------------------------------------------------------
manage_senders(Senders, CompSendersMap) ->
	receive
		{company, Comp, From} ->
			case maps:find(Comp, CompSendersMap) of
				{ok, Sender} -> 
					Pid = maps:get(Sender, Senders),
					From ! {ok, Pid};
				error ->
					From ! non_existent_company
			end,
			manage_senders(Senders, CompSendersMap)
	end.