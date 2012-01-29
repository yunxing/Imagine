-module(room).
-export([start/0]).

start()->
    Tab = ets:new(table, [set]),
    initializeTable(Tab, 0),
    loop([], Tab, lists:seq(0,4)).

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

loop(Q, Tab, E)->
    receive
	{Pid, {join, Waiter, Email, Sex}}->
	    case E of
		[] ->
		    Pid!{fail, full},
		    loop(Q, Tab, E);
		[T|R] ->
		    Waiter ! {self(), welcome, {id, T}},
		    ets:insert(Tab, {T, Email, Sex, online}),
		    initializeWaiter(Waiter, 0, Tab),
		    sendAll([Waiter|Q], T, online, update),
		    Pid!ok,
		    if
			length(Q) == 4 ->
			    Pid!{self(), detach},
			    Total = 1 * 10,
			    timer:send_after(Total * 1000,
				       self(),
				       {timeup}),
			    notifyAll([Waiter|Q], started, Total);
			true -> true
		    end,
		    loop([Waiter|Q], Tab, R)
	    end;
	{say, ID, DATA} ->
	    sendAll(Q, ID, DATA, say),
	    loop(Q, Tab, E);
	{timeup} ->
	    io:format("time up~n"),
	    Total = 1 * 2 * 1000,
	    timer:send_after(Total, self(), {timeup}),
	    selLoop(Q, Tab, E, [], false);
	{update, ID, Status}->
	    [{_, Email, Sex, _}] = ets:lookup(Tab, ID),
	    ets:insert(Tab, {ID, Email, Sex, Status}),
	    sendAll(Q, ID, Status, update),
	    loop(Q, Tab, E);
	{Pid, closed, ID} ->
	    NewQ = lists:delete(Pid, Q),
	    case length(NewQ) of
		0 ->
		    manager!{self(), detach},
		    io:format("Room deleted~n"),
		    closed;
		_N ->
		    loop(NewQ, Tab, [ID | E])
	    end
    end.

selLoop(Q, Tab, E, P, T)->
    receive
	{timeup} ->
	    notifyAll(Q, endSel, T),
	    closed;
	{choice, ID, ARG}->
	    case lists:member({ARG, ID}, P) of
		true ->
		    [{ARG, EmailARG, SexARG, _}]=
			ets:lookup(Tab, ARG),
		    [{ID, EmailID, SexID, _}]=
			ets:lookup(Tab, ID),
		    mail ! {sendmail, EmailARG, SexARG, EmailID, SexID},
		    notifyAll(Q, pair,
			      {{ARG, EmailARG, SexARG},
			       {ID, EmailID, SexID}}),
		    NewP = lists:delete({ARG, ID}, P),

		    selLoop(Q, Tab, E, NewP, true);
		false ->
		    notifyAll(Q, closed, {}),
		    selLoop(Q, Tab, E, [{ID, ARG}|P], T)
	    end
    end.
