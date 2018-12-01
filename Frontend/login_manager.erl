-module(login_manager).
-export([login/2,logout/1,online/0,loop/1,start/0, rpc/1]).

rpc(Req) ->
	%podiamos tb ter metido ?MODULE ! {Req, self()} e no loop fazer os 
	%casos com tuplos dentro de tuplos
	ReqU = erlang:insert_element(tuple_size(Req) + 1,Req,self()),
	?MODULE ! ReqU,
	receive{?MODULE, Rep} -> Rep end.

-spec login(Username :: term(), Passwd :: term()) -> {ok,Client_T :: term()} | invalid.
login(Username, Passwd) ->
	rpc({login, Username, Passwd}).

-spec logout(Username :: term()) -> ok | invalid.
logout(Username) ->
	rpc({logout, Username}).

-spec online() -> [Username :: term()].
online() ->
	rpc({online}).

start() ->
	Clients = #{
		"empA" => {"pw_empA", company, false},
		"empB" => {"pw_empB", company, false},
		"empC" => {"pw_empC", company, false},
		"invA" => {"pw_invA", investor, false},
		"invB" => {"pw_invB", investor, false},
		"invC" => {"pw_invC", investor, false},
		"invD" => {"pw_invD", investor, false},
		"invE" => {"pw_invE", investor, false}
	},

	register(?MODULE, spawn(fun() -> login_manager:loop(Clients) end)). % #{} -> notação built-in para mapa vazio
															% ou loop(maps:new())

loop(Map) -> 
	receive
		{login, Username, Passwd, From} ->
			case maps:find(Username, Map) of
				{ok, {Passwd, TipoCliente, false}} -> % se o utilizador existir e não estiver logged in
					io:fwrite("Tentativa de Login ~n",[]),
					From ! {login_manager, {ok, TipoCliente}},
					loop(maps:put(Username, {Passwd, TipoCliente, true}, Map));
				_ ->
					io:fwrite("Tentativa de Login Falhada ~n",[]),
					From ! {login_manager, invalid},
					loop(Map)
			end;
		{logout, Username, From} ->
			case maps:find(Username, Map) of
				{ok, {Passwd, TipoCliente, true}} -> % se o utilizador existir e estiver logged in
					From ! {login_manager, ok},
					loop(maps:put(Username, {Passwd, TipoCliente, false}, Map));
				_ ->
					From ! {login_manager, invalid},
					loop(Map)
			end;
		{online, From} ->
			Fun = fun(K,{_, _,ON_OFF}, ResP) when ON_OFF =:= true -> [K|ResP];
					(_, _, ResP) -> ResP end,
			Res = maps:fold(Fun, [], Map),
			From ! {login_manager, Res},
			loop(Map)
		
	end.


