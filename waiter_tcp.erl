-module(waiter_tcp).

-export([start/1]).

start({Server, S})->
    case gen_tcp:accept(S) of
	{ok, Socket} ->
	    Server ! next_worker,
	    Self = self(),
	    spawn_link(fun() ->messenger(Self, Socket) end),
	    {Email, Sex} = waitForEmailAndSex(),
	    manager ! {self(), {join, {Email, Sex}}},
	    io:format("waiting for room assignment~n"),
	    
	    receive
		{Room, welcome, {id, ID}} ->
		    io:format("get room:~p, ID:~p~n", [Room, ID]),
		    send(Socket, ["/joined/" , integer_to_list(ID)]),
		    handle_websocket(Socket, ID, Room)
	    end;
	{error, Reason} ->
	    Server ! next_worker,
	    io:format("Can't accept socket ~p~n", [Reason])
    end.

messenger(P, Socket)->
    case gen_tcp:recv(Socket, 0) of    
	{ok, Raw}->
	    Data = binary_to_list(Raw),
	    P ! {socket, Data},
	    messenger(P, Socket);
	Other->
	    io:format("socket received:~p~n", [Other]),
	    P ! closed
    end.

waitForEmailAndSex()->
    receive
	{socket, Data} ->
	    [_, _, Email, Sex] = re:split(Data, "/"),
	    {Email, Sex};
	Other -> io:format("wrong register ~p~n", [Other])
    end.

handle_websocket(Ws, ID, Room)->
    receive
	{socket, Data}->
	    io:format("from client:~p~n", [Data]),
	    case re:run(Data, "^/(?<CMD>.*?)/\n?(?<ARG>.*)", [dotall, {capture, ['CMD','ARG'], list}]) of
		{match, [CMD, ARG]}->
		    case list_to_atom(CMD) of
			say->
			    Room ! {say, ID, ARG};
			status ->
			    Room ! {update, ID, list_to_atom(ARG)};
			choice ->
			    io:format("get selecton~n"),
			    Room ! {choice, ID, list_to_integer(ARG)},
			    io:format("sendSelecttion~n")
		    end,
		    handle_websocket(Ws, ID, Room);
		nomatch->
		    io:format("unknown regular command:~p~n",[Data]),
		    closed
	    end;
	{say, SID, DATA} ->
	    send(Ws, ["/say/", integer_to_list(SID), "/", DATA]),
	    handle_websocket(Ws, ID, Room);
	{update, SID, Status} ->
	    timer:sleep(200),
	    send(Ws, ["/status/", integer_to_list(SID), "/", atom_to_list(Status)]),
	    handle_websocket(Ws, ID, Room);
	{system, CMD, ARG} ->
	    case CMD of
		nearEnd->
		    send(Ws, ["/event/nearEnd"]),
		    send(Ws, ["/say/sys/Make a selection now!"]),
		    handle_websocket(Ws, ID, Room);
		started->
		    send(Ws,["/say/",
				     "sys",
				     "/",
				     "The door has closed! Let's chat!"]),
		    send(Ws,["/event/",
				     "started/",
				     integer_to_list(ARG)
				    ]),
		    handle_websocket(Ws, ID, Room);
		pair->
		    {{A, EmailA, SexA}, {B, EmailB, SexB}} = ARG,
		    if
			A == ID ->
			    send(Ws,["/event/",
					     "info/",
					     integer_to_list(B),
					     "/",
					     EmailB,
					     "/",
					     SexB]
					);
			B == ID ->
			    send(Ws,["/event/",
					     "info/",
					     integer_to_list(A),
					     "/",
					     EmailA,
					     "/",
					     SexA]
					);
			true -> true
		    end,
		    send(Ws, ["/event/",
				  "paired/",
				  integer_to_list(A),
				  "/",
				  integer_to_list(B)]
				),
		    handle_websocket(Ws, ID, Room);
		endSel->
		    send(Ws, ["/event/",
				  "end"
				 ]
				),
		    case ARG of
			false ->
			    send(Ws, ["/say/",
					  "sys/",
					  "no pair matched, what a pity!"
					 ]
					);
			true -> ok
		    end
	    end;
	closed ->
	    io:format("user exited~n"),
	    Room ! {update, ID, offline}, 
	    Room ! {self(), closed, ID};
	Other ->
	    io:format("unknown command!~p~n", [Other]),
	    handle_websocket(Ws, ID, Room)
    end.

send(_Ws, []) ->
    ok;
send(Ws, Data) ->
    io:format("sent to client:~p~n", [Data]),
    gen_tcp:send(Ws, Data). 
