%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, Paradigmas de Sistemas Distribuídos
%%% @doc Modulo que possui a API necessária à inicialização do Frontend
%%%
%%% Responsável por iniciar a aplicação Frontend, aceitar conexões de clientes
%%% e proceder à sua autenticação.
%%% @author Grupo 3
%%% @end
%%%-----------------------------------------------------------------------------
-module(frontend_app).

-behaviour(application).
-include("protos_pb.hrl").

%% Application callbacks
-export([start/2, stop/1, server/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    frontend_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%------------------------------------------------------------------------------
%% @doc Função que inicializa a aplicação FrontEnd.
%%
%% Cria um ator responsável por manter registo dos utilizadores e autenticações,
%% como também os Senders e Receivers associados a cada Exchange. Invoca a
%% função responsável por aceitar conexões dos clientes.
%% @end
%%------------------------------------------------------------------------------
server(Port) -> 
	login_manager:start(),
	senders_manager:start(),
	receiver:start("127.0.0.1", "1221"),
	receiver:start("127.0.0.1", "1222"),
	receiver:start("127.0.0.1", "1223"),
	{ok, LSock} = gen_tcp:listen(Port, [binary,{packet, 0}, {reuseaddr, true}, {active, true}]),
	acceptor(LSock).

%%====================================================================
%% Internal functions
%%====================================================================

%%------------------------------------------------------------------------------
%% @doc Função que lida com o estabelecimento de conexões.
%%
%% Aguarda a conexão de um cliente, quando estabelecida encarrega-se de o
%% autenticar e aceitar simultanemente novas conexões.
%% @end
%%------------------------------------------------------------------------------
acceptor(LSock) ->
	% Espero a conexão de um cliente
	{ok, Sock} = gen_tcp:accept(LSock),
	% Crio um novo processo que vai aceitar mais clientes
	spawn(fun() -> acceptor(LSock) end),
	authenticate(Sock).

%%------------------------------------------------------------------------------
%% @doc Função que autentica um cliente e cria um ator para a sua sessão.
%%
%% Enquanto o cliente não se encontra autentica aguarda por pedidos de login.
%% Para cada pedido verifica as credenciais e autentica o cliente, mudando de
%% contexto para servir o cliente, em caso de login efetuado.
%% @end
%%------------------------------------------------------------------------------
authenticate(Sock) ->
	receive
		{tcp, Sock, Bin} ->
			{'MessageWrapper', _, _, Msg} = protos_pb:decode_msg(Bin, 'MessageWrapper'),
			{loginreq, {'LoginReq', User_B, Pass_B}} = Msg,			
			User = binary_to_list(User_B),
			Pass = binary_to_list(Pass_B),

			case login_manager:login(User, Pass) of
				{ok, company} ->
					LoginResp = #'LoginResp'{cType='COMPANY', status='SUCCESS'},
					Token = pid_to_list(self()),
					Resp = protos_pb:encode_msg(#'MessageWrapper'{msgType = 'SYNC', clientSession = Token, inner_message = {loginresp, LoginResp}}),
					gen_tcp:send(Sock, Resp),
					client_session:client_loop(Sock, User);
				{ok, investor} ->
					LoginResp = #'LoginResp'{cType='INVESTOR', status='SUCCESS'},
					Token = pid_to_list(self()),
					Resp = protos_pb:encode_msg(#'MessageWrapper'{msgType = 'SYNC', clientSession = Token, inner_message = {loginresp, LoginResp}}),
					gen_tcp:send(Sock, Resp),
					client_session:client_loop(Sock, User);
				invalid ->
					LoginResp = #'LoginResp'{status='INVALID'},
					Resp = protos_pb:encode_msg(#'MessageWrapper'{msgType = 'SYNC', inner_message = {loginresp, LoginResp}}),
					gen_tcp:send(Sock, Resp),
					authenticate(Sock)
			end
	end.

