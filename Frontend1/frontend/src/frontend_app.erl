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
	receiver:start("127.0.0.1", "1222"),
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
					client_session:client_loop(Sock, User);
					% company(Sock);
				{ok, investor} ->
					LoginResp = #'LoginResp'{cType='INVESTOR', status='SUCCESS'},
					Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {loginresp, LoginResp}}),
					gen_tcp:send(Sock, Resp),
					client_session:client_loop(Sock, User);
					% investor(Sock);
				invalid ->
					LoginResp = #'LoginResp'{status='INVALID'},
					Resp = protos_pb:encode_msg(#'MessageWrapper'{inner_message = {loginresp, LoginResp}}),
					gen_tcp:send(Sock, Resp),
					authenticate(Sock)
			end
	end.

