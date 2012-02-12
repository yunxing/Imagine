-module(tcpgateway).
-export([start/0]).
-define(PORTNO, 12420).

start()->
    io:format("tcp gateway, at your service~n"),
    case gen_tcp:listen(?PORTNO, [binary, {reuseaddr, true},
			       {packet, 0}, {active, false}]) of
	{ok, LSock} ->
	    spawn_link(waiter_tcp, start, [{self(), LSock}]),
	    loop(LSock);
	Other ->
	    io:format("can't listen due to ~p !~n", [Other])
    end.

loop(S)->
    receive
	next_worker->
	    io:format("tcp:new customer!~n"),
	    spawn_link(waiter_tcp, start, [{self(), S}])
    end,
    loop(S).








    
