-module(login_manager).
-export([login/2,logout/1,online/0,loop/1,start/0,get_pid/1,rpc/1]).

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

get_pid(Username) ->
	rpc({pid, Username}).

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

loop(Map) -> 
	receive
		{login, Username, Passwd, From} ->
			case maps:find(Username, Map) of
				{ok, {Passwd, TipoCliente, false, _ }} -> % se o utilizador existir e não estiver logged in
					io:fwrite("Tentativa de Login ~n",[]),
					From ! {login_manager, {ok, TipoCliente}},
					loop(maps:put(Username, {Passwd, TipoCliente, true, From}, Map));
				_ ->
					io:fwrite("Tentativa de Login Falhada ~n",[]),
					From ! {login_manager, invalid},
					loop(Map)
			end;
		{logout, Username, From} ->
			case maps:find(Username, Map) of
				{ok, {Passwd, TipoCliente, true, _}} -> % se o utilizador existir e estiver logged in
					From ! {login_manager, ok},
					loop(maps:put(Username, {Passwd, TipoCliente, false, null}, Map));
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


