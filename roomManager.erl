-module(roomManager).
-export([start/0]).

start()->
    loop([], 0).

askJoin([], _)-> false;
askJoin([T|Q], {Pid, Email, Sex}) ->
    case remote:rpc(T, {join, Pid, Email, Sex}) of
	ok->
	    ok;
	{fail, _} ->
	    askJoin(Q, {Pid, Email, Sex})
    end.

loop(Q, N)->
    receive
	{Pid, {join, {Email, Sex}}}->
	    case askJoin(Q, {Pid, Email, Sex}) of
		false->
		    io:format("spawning a new room, number:~p~n", [N]),
		    T = spawn_link(fun() -> room:start() end),
		    askJoin([T|Q], {Pid, Email, Sex}),
		    loop([T|Q], N + 1);
		ok ->
		    loop(Q, N)
	    end;
	{Pid, detach}->
	    NewQ = lists:delete(Pid, Q),
	    io:format("room detached:~p~n",[Pid]),
	    loop(NewQ, N)
    end.
