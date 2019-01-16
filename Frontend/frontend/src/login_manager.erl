%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, Paradigmas de Sistemas Distribuídos
%%% @doc Modulo que possui e manipula o estado relativo aos utilizadores. 
%%%
%%% Responsável por manter registo dos utilizadores existentes e por proceder
%%% à sua autenticação e desautenticaçao.
%%% @author Grupo 3
%%% @end
%%%-----------------------------------------------------------------------------
-module(login_manager).
-export([login/2,logout/1,online/0,loop/1,start/0,get_pid/1,rpc/1]).

%%------------------------------------------------------------------------------
%% @doc Função que representa um RPC útil na implementação das restantes funções. 
%%
%% Envia uma mensagem de pedido ao ator responsável pelo autenticação dos 
%% utilizadores.
%% @end
%%------------------------------------------------------------------------------
rpc(Req) ->
	%podiamos tb ter metido ?MODULE ! {Req, self()} e no loop fazer os 
	%casos com tuplos dentro de tuplos
	ReqU = erlang:insert_element(tuple_size(Req) + 1,Req,self()),
	?MODULE ! ReqU,
	receive{?MODULE, Rep} -> Rep end.

%%------------------------------------------------------------------------------
%% @doc Função invoca um procedimento remoto responsável pela autenticação 
%% dos utilizadores.
%% @end
%%------------------------------------------------------------------------------
-spec login(Username :: term(), Passwd :: term()) -> {ok,Client_T :: term()} | invalid.
login(Username, Passwd) ->
	rpc({login, Username, Passwd}).

%%------------------------------------------------------------------------------
%% @doc Função invoca um procedimento remoto responsável pela desautenticação 
%% dos utilizadores.
%% @end
%%------------------------------------------------------------------------------
-spec logout(Username :: term()) -> ok | invalid.
logout(Username) ->
	rpc({logout, Username}).

%%------------------------------------------------------------------------------
%% @doc Função invoca um procedimento remoto responsável por verificar quais 
%% os utilizadores autenticados.
%% @end
%%------------------------------------------------------------------------------
-spec online() -> [Username :: term()].
online() ->
	rpc({online}).

%%------------------------------------------------------------------------------
%% @doc Função invoca um procedimento remoto responsável pela obtenção do PID
%% do ator representativo da sessão do cliente.
%% @end
%%------------------------------------------------------------------------------
get_pid(Username) ->
	rpc({pid, Username}).

%%------------------------------------------------------------------------------
%% @doc Função que inicializa o estado do ator representativo do login_manager,
%% com a informação das empresas e investidores existentes.
%% @end
%%------------------------------------------------------------------------------
start() ->
	Clients = #{
		"empA" => {"pw_empA", company, false, null},
		"empB" => {"pw_empB", company, false, null},
		"empC" => {"pw_empC", company, false, null},
		"empD" => {"pw_empD", company, false, null},
		"invA" => {"pw_invA", investor, false, null},
		"invB" => {"pw_invB", investor, false, null},
		"invC" => {"pw_invC", investor, false, null},
		"invD" => {"pw_invD", investor, false, null},
		"invE" => {"pw_invE", investor, false, null}
	},

	register(?MODULE, spawn(fun() -> login_manager:loop(Clients) end)). % #{} -> notação built-in para mapa vazio
															% ou loop(maps:new())


%%------------------------------------------------------------------------------
%% @doc Função responsável por processar os pedidos de manipulação de estado
%% dos utilizadores.
%%
%% Permite autenticar/desautenticar utilizadores e devolver o pid do ator 
%% responsável pela sessão de um utilizador autenticado.
%% @end
%%------------------------------------------------------------------------------
loop(Map) -> 
	receive
		{login, Username, Passwd, From} ->
			case maps:find(Username, Map) of
				{ok, {Passwd, TipoCliente, false, _ }} -> % se o utilizador existir e não estiver logged in
					From ! {login_manager, {ok, TipoCliente}},
					loop(maps:put(Username, {Passwd, TipoCliente, true, From}, Map));
				_ ->
					From ! {login_manager, invalid},
					loop(Map)
			end;
		{logout, Username, From} ->
			case maps:find(Username, Map) of
				{ok, {Passwd, TipoCliente, true, _}} -> % se o utilizador existir e estiver logged in
					From ! {login_manager, ok},
					loop(maps:put(Username, {Passwd, TipoCliente, false, null}, Map)),
					io:format("Logged out!! ~n", []);
				_ ->
					From ! {login_manager, invalid},
					loop(Map)
			end;
		{online, From} ->
			Fun = fun(K,{_, _,ON_OFF, _}, ResP) when ON_OFF =:= true -> [K|ResP];
					(_, _, ResP) -> ResP end,
			Res = maps:fold(Fun, [], Map),
			From ! {login_manager, Res},
			loop(Map);
		{pid, Username, From} ->
			case maps:find(Username, Map) of
				{ok, {_, _, true, Pid}} ->
					From ! {login_manager, Pid},
					loop(Map);
				_ ->
					From ! {login_manager, invalid},
					loop(Map)
			end
		
	end.


