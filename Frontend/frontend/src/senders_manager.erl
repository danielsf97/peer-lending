-module(senders_manager).
-export([start/0]).

start() ->
	Senders = create_senders(),
	CompSendersMap = get_comp_senders_mapping(),
	register(?MODULE, spawn ( fun() -> manage_senders(Senders, CompSendersMap) end) ).

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

get_comp_senders_mapping() ->
	CompSendersMap = #{
		"empA" => sender1,
		"empB" => sender1,
		"empC" => sender2,
		"empD" => sender3
	},
	CompSendersMap.

manage_senders(Senders, CompSendersMap) ->
	receive
		{company, Comp, From} ->
			case maps:find(Comp, CompSendersMap) of
				{ok, Sender} -> 
					Pid = maps:get(Sender, Senders),
					From ! {ok, Pid};
				error ->
					non_existent_company
			end,
			manage_senders(Senders, CompSendersMap)
	end.