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
	{ok, Bin} = do_recv(Sock, []),
	Msg = protos:decode_msg(Bin, 'LoginReq'),
	Msg.


%	receive
%		{tcp, Sock, Data} -> 
%			Data,
%			protos:decode_msg(Data, 'LoginReq')
%
%	end.


do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.