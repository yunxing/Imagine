-module(imagine).
-export([restart/0, start/0, stop/0]).

restart() ->
    stop(),
    timer:sleep(500),
    start().

stop() ->
    misultin:stop().

start() ->
    misultin:start_link([{port, 8080},
			 {loop, fun(Req) -> handle_http(Req) end},
			 {ws_loop, fun(Ws) -> handle_websocket(Ws) end},
			 {ws_autoexit, false}
			]),
    register(queue, spawn_link(fun() -> startQueue() end)),
    register(tableManager, spawn_link(fun() -> startTable() end)).


getStatus([], [], Acc) ->
    Acc;
getStatus([], [T|E], Acc) ->
    getStatus([], E, [{T, "offline"}|Acc]);
getStatus([{T, _W}|Q], E, Acc) ->
    getStatus(Q, E, [{T, "<font color='blue'>online"}|Acc]).

startTable()->
    Tab = ets:new(table, [set]),
    tloop(Tab),
    ets:delete(Tab).

rpc(Process, Req)->
    Process ! {self(),Req},
    receive
	R -> R
    end.

tloop(Tab)->
    receive
	{Pid, {insert, ID, Name, Status}} ->
	    Pid ! ets:insert(Tab, {ID, Name, Status}),
	    tloop(Tab);
	{Pid, {lookup, ID}} ->
	    Pid ! ets:lookup(Tab, ID),
	    tloop(Tab);
	{Pid, {del, ID}} ->
	    Pid ! ets:delete(Tab, ID),
	    tloop(Tab);
	{close} ->
	    closed
    end.


startQueue()->
    loop([], lists:seq(1,8)).


loop(Q, E) ->
    receive
	{Pid, who} ->
	    S = getStatus(Q, E, []),
	    Pid ! {ok, S},
	    loop(Q, E);
	{Pid, add, Ws} ->
	    case E of
		[] -> Pid ! full,
		      loop(Q, E);
		_  ->
		    {NewQ, NewE} = add(Q, E, Ws),
		    [{T, _W}|_] = NewQ,
		    Pid ! {ok, T},
		    loop(NewQ, NewE)
	    end;
	{update, ID, Name, Status}->
	    case Status of
		online->
		    R = rpc(tableManager, {lookup, ID}),
		    rpc(tableManager, {insert, ID, Name, online}),
		    sendAll(["/status/", integer_to_list(ID), "/", "<font color='blue'>online", "/", Name], Q),
		    case R of
			[] -> 
			    sendAll(["<font size='3' color='blue'> ",
				     Name,
				     " has joined the server</font>"],
				    Q);
			_ -> true
		    end;
		away->
		    rpc(tableManager, {insert, ID, Name, away}),
		    sendAll(["/status/", integer_to_list(ID), "/", "<font color='yellow'>away", "/", Name], Q)
	    end,
	    loop(Q, E);
	{delete, Num} ->
	    {NewQ, NewE} = del(Q, E, Num),
	    [{_,Name, _}] = rpc(tableManager, {lookup, Num}),
	    rpc(tableManager, {del, Num}),
	    sendAll(["/status/", integer_to_list(Num), "/", "offline", "/", "empty"], NewQ),	    
	    sendAll(["<font size='3' color='blue'> ",
		     Name,
		     " has left the server</font>"],
		    NewQ),
	    loop(NewQ, NewE);
	{send, Msg, Num} ->
	    [{_,R, _}] = rpc(tableManager, {lookup, Num}),
	    sendAll([R,
		     " : ", Msg],
		    Q),
	    loop(Q, E);
	Command ->
	    io:format("unknown command:~p~n", [Command])
    end.

sendAll(_, []) -> true;
sendAll(Msg, [{_T, W}|Q]) ->
    W:send([Msg]),
    sendAll(Msg, Q).

del(Q, E, Num) ->
    del(Q, E, Num, []).

del([], E, _, Acc) -> {Acc, E};
del([{T, W}|Q], E, Num, Acc) ->
    case T of
	Num -> del(Q, [Num|E], Num, Acc);
	_ -> del(Q, E, Num, [{T, W}|Acc])
    end.

add(Q, [T|E], Ws) ->
    {[{T, Ws}|Q], E}.


handle_http(Req) ->
    handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).


handle('GET', [], Req) ->
    erlydtl:compile("./static/index.html", index),
    {ok, Index} = index:render([]),
    Req:ok(Index);
handle('GET', ["chat"], Req) ->
    {ok, [{addr, Addr}]} = inet:ifget("wlan0", [addr]),
    Args = Req:parse_qs(),
    case proplists:get_value("user", Args) of
	undefined -> User = "auto";
	Value -> User = Value
    end,
    erlydtl:compile("./template/chat.html", chat),
    {ok, Chat} = chat:render([
			      {title, "Imagine-0.0.7"},
			      {ip, inet_parse:ntoa(Addr)},
			      {user, User}
			     ]),
    Req:ok([Chat]);    
handle(_, Other, Req) ->
    case filelib:is_file(["./static/", Other]) of
	true->
	    Req:file(["./static/", Other]);
	_ ->
	    case filelib:is_file(["./static/", Other, ".html"]) of
		true->
		    Req:file(["./static/", Other, ".html"]);
		_ ->
		    Req:ok([{"Content-Type", "text/plain"}], "Page not found.")
	    end
    end.

getID(Ws) ->
    queue ! {self(), add, Ws},
    receive
	{ok,ID} ->
	    ID;
	full -> full
    end.

getList() ->
    queue ! {self(), who},
    receive
	{ok, L} -> L
    end.

updateStatus({Who, Status}, Ws)->
    case rpc(tableManager, {lookup, Who}) of
	[{_, R, S}] ->
	    case S of
		away->
		    Ws:send(["/status/", integer_to_list(Who), "/<font color='yellow'>away", "/", R]);
		online->
		    Ws:send(["/status/", integer_to_list(Who), "/<font color='blue'>online", "/", R])
	    end;
	_ -> Ws:send(["/status/", integer_to_list(Who), "/", Status, "/", "empty"])
    end.


updateList([], _Ws)->
    ok;
updateList([T|List], Ws) ->
    updateStatus(T, Ws),
    updateList(List, Ws).

waitForName(ID)->
    receive
	{browser, Data} ->
	    [_, _, Name] = re:split(Data, "/"),
	    case binary_to_list(Name) of
		"auto" -> ["Guest ", integer_to_list(ID)];
		Other -> Other
	    end;
	Other -> io:format("wrong register ~p~n", [Other])
    end.

handle_websocket(Ws) ->
    case getID(Ws) of
	full -> Ws:send("server is full, wait for next round!"),
		closed;
	ID ->
	    Name = waitForName(ID),
	    Who = getList(),
	    queue ! {update, ID, Name, online},
	    updateList(Who, Ws),
	    handle_websocket(Ws, ID, Name)
    end.

handle_websocket(Ws, ID, Name) ->
    receive
	{browser, Data} ->
	    case re:run(Data, "^/(?<CMD>.*?)/\n?(?<ARG>.*)", [dotall, {capture, ['CMD','ARG'], list}]) of
		{match, [CMD, ARG]}->
		    case list_to_atom(CMD) of
			say->
			    queue ! {send, ARG, ID};
			status ->
			    queue ! {update, ID, Name, list_to_atom(ARG)}
		    end,
		    handle_websocket(Ws, ID, Name);
		nomatch->
		    io:format("unknown command:~p~n",[Data]),
		    closed
	    end;
	closed ->
	    queue ! {delete, ID}
    end.
