-module(room).
-export([start/0]).

start()->
    Tab = ets:new(table, [set]),
    initializeTable(Tab, 0),
    loop([], Tab, lists:seq(0,4), []).

initializeTable(_Tab, 5) -> ok;
initializeTable(Tab, N) ->
    ets:insert(Tab, {N, "N/A", "N/A", offline}),
    initializeTable(Tab, N + 1).

initializeWaiter(_Waiter, 5, _Tab) -> ok;
initializeWaiter(Waiter, N, Tab) ->
    [{ID, _, _, Status}]= ets:lookup(Tab, N),
    Waiter ! {update, ID, Status},
    initializeWaiter(Waiter, N + 1, Tab).
sendAll([], _, _, _)->
    ok;
sendAll([T|Q], ID, DATA, CMD)->
    T ! {CMD, ID, DATA},
    sendAll(Q, ID, DATA, CMD).

notifyAll([], _, _)->
    ok;
notifyAll([T|Q], CMD, ARG)->
    T ! {system, CMD, ARG},
    notifyAll(Q, CMD, ARG).

loop(Q, Tab, E, P)->
    receive
	{Pid, {join, Waiter, Email, Sex}}->
	    case E of
		[] ->
		    Pid!{fail, full},
		    loop(Q, Tab, E, P);
		[T|R] ->
		    Waiter ! {self(), welcome, {id, T}},
		    ets:insert(Tab, {T, Email, Sex, online}),
		    initializeWaiter(Waiter, 0, Tab),
		    sendAll([Waiter|Q], T, online, update),		    
		    Pid!ok,
		    if
			length(Q) == 4 ->
			    Pid!{self(), detach},
			    notifyAll([Waiter|Q], started, []);
			true -> true
		    end,		    
		    loop([Waiter|Q], Tab, R, P)
	    end;
	{say, ID, DATA} ->
	    sendAll(Q, ID, DATA, say),
	    loop(Q, Tab, E, P);
	{update, ID, Status}->
	    [{_, Email, Sex, _}] = ets:lookup(Tab, ID),
	    ets:insert(Tab, {ID, Email, Sex, Status}),
	    sendAll(Q, ID, Status, update),
	    loop(Q, Tab, E, P);
	{choice, ID, ARG}->
	    case lists:member({ARG, ID}, P) of
		true->
		    notifyAll(Q, pair, {ARG, ID}),
		    NewP = lists:delete({ARG, ID}, P),
		    loop(Q, Tab, E, NewP);
		false ->
		    loop(Q, Tab, E, [{ID, ARG}|P])
	    end;
	{Pid, closed, ID} ->
	    NewQ = lists:delete(Pid, Q),
	    case length(NewQ) of
		0 ->
		    manager!{self(), detach},
		    io:format("Room deleted~n"),
		    closed;
		_N ->
		    loop(NewQ, Tab, [ID | E], P)
	    end
    end.

