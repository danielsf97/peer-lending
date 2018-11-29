-module(frontend).
-export([server/1]).
-include("protos.hrl").

server(Port) -> 
	login_manager:start(),
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, {active, false}]),
	acceptor(LSock).

acceptor(LSock) ->
	% Espero a conexão de um cliente
	{ok, Sock} = gen_tcp:accept(LSock),
	% Crio um novo processo que vai aceitar mais clientes
	spawn(fun() -> acceptor(LSock) end),
	authenticate(Sock).

company(Sock) ->
	true.

investor(Sock)->
	true.

authenticate(Sock) ->
	io:fwrite("Start Authentication ~n",[]),
	Bin = recv(Sock),
	io:fwrite("Received Authentication Msg ~n",[]),
	{'LoginReq', User, Pass} = protos:decode_msg(Bin, 'LoginReq'),
	io:fwrite("Decoded msg ~n",[]),
	case login_manager:login(User, Pass) of
		{ok, company} ->
			Resp = protos:encode_msg(#'LoginRep'{cType='COMPANY', status='SUCCESS'}),
			gen_tcp:send(Sock, Resp),
			company(Sock);
		{ok, investor} ->
			Resp = protos:encode_msg(#'LoginRep'{cType='INVESTOR', status='SUCCESS'}),
			gen_tcp:send(Sock, Resp),
			investor(Sock);
		invalid ->
			Resp = protos:encode_msg(#'LoginRep'{status='INVALID'}),
			gen_tcp:send(Sock, Resp),
			authenticate(Sock)
	end.

recv(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            B;
        {error, closed} ->
            error
    end.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.