%%%-------------------------------------------------------------------
%% @doc frontend public API
%% @end
%%%-------------------------------------------------------------------

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

%%--------------------------------------------------------------------

server(Port) -> 
	login_manager:start(),
	senders_manager:start(),
	% receiver:start("localhost", 1222),
	{ok, LSock} = gen_tcp:listen(Port, [binary,{packet, 0}, {reuseaddr, true}, {active, true}]),
	acceptor(LSock).

%%====================================================================
%% Internal functions
%%====================================================================

acceptor(LSock) ->
	% Espero a conexão de um cliente
	{ok, Sock} = gen_tcp:accept(LSock),
	% Crio um novo processo que vai aceitar mais clientes
	spawn(fun() -> acceptor(LSock) end),
	authenticate(Sock).

% company(Sock) ->
% 	true.

% investor(Sock) ->
% 	CompExchangeMap = get_exchanges(),
% 	{SocketsPush, SocketPull} = create_investor_topology(CompExchangeMap),
% 	spawn(fun() -> exchange_listener(Sock, SocketPull) end),
% 	investor_listener(Sock, SocketsPush).

% get_exchanges() ->
% 	CompExchangeMap = #{
% 	%	empresa -> exchange ports Pull/Push
% 		"empA" => { "1241", "1251"},
% 		"empB" => { "1242", "1252"},
% 		"empC" => { "1243", "1253"}
% 	},
% 	CompExchangeMap.

% create_investor_topology(Map) ->
% 	% criação de um contexto
% 	{ok,Context} = erlzmq:context(),
% 	% função que para cada empresa adiciona ao acumulador 
% 	% um socket de conexão ao Pull da exchange
% 	FunPush = fun(EMP, {PULL, PUSH}, Acc) ->
% 		{ok, Sender} = erlzmq:socket(Context, push),
% 		ok = erlzmq:connect(Sender, "tcp://localhost:" ++ PULL),
% 		maps:put(EMP,Sender,Acc) end,
% 	% função que para cada empresa faz com que o Socket receiver
% 	% se conecte a mais uma Exchange (Atençao!! a alterar porque
% 	% haverão empresas mapeadas na mesma exchange)
% 	FunPull = fun(EMP, {PULL, PUSH}, Receiver) ->
% 		ok = erlzmq:connect(Receiver, "tcp://localhost:" ++ PUSH),
% 		Receiver end,
% 	% Criação do mapeamento empresa/socket push
% 	SocketsPush = maps:fold(FunPush, #{}, Map),
% 	% Criação de um socket conetado a todas as exchanges
% 	{ok ,Receiver} = erlzmq:socket(Context, pull),
% 	SocketPull = maps:fold(FunPull, Receiver, Map),

% 	{SocketsPush, SocketPull}.

% exchange_listener(Sock, SocketPull) ->
% 	{ok, Bin} = erlzmq:recv(SocketPull),
% 	gen_tcp:send(Sock, Bin).
	

% investor_listener(Sock, SocketsPush) ->
% 	Bin = recv(Sock),
% 	Bin,
% 	{'InvestidorAcaoPed', Comp_B, ReqT_B, Value_B, Rate_B} = 
% 			protos_pb:decode_msg(Bin, 'InvestidorAcaoPed'),
% 	Comp = binary_to_list(Comp_B),
% 	SocketSend = maps:get(Comp, SocketsPush),
% 	ok = erlzmq:send(SocketSend, Bin),
% 	io:fwrite("Message Sent to company " ++ Comp ++ "~n",[]),
% 	investor_listener(Sock, SocketsPush).


authenticate(Sock) ->
	io:fwrite("Start Authentication ~n",[]),
	receive
		{tcp, Sock, Bin} ->
			{'MessageWrapper', Msg} = protos_pb:decode_msg(Bin, 'MessageWrapper'),
			{loginreq, {'LoginReq', User_B, Pass_B}} = Msg,			
			User = binary_to_list(User_B),
			Pass = binary_to_list(Pass_B),

			case login_manager:login(User, Pass) of
				{ok, company} ->
					LoginResp = #'LoginResp'{cType='COMPANY', status='SUCCESS'},
					Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {loginresp, LoginResp}}),
					gen_tcp:send(Sock, Resp),
					client_session:client_loop(Sock);
					% company(Sock);
				{ok, investor} ->
					LoginResp = #'LoginResp'{cType='INVESTOR', status='SUCCESS'},
					Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {loginresp, LoginResp}}),
					gen_tcp:send(Sock, Resp),
					client_session:client_loop(Sock);
					% investor(Sock);
				invalid ->
					LoginResp = #'LoginResp'{status='INVALID'},
					Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {loginresp, LoginResp}}),
					gen_tcp:send(Sock, Resp),
					authenticate(Sock)
			end
	end.

