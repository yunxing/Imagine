-module(gmail).
-export([start/0, sendMail/4]).


start() ->
    io:format("gmail sender at your service~n"),
    ssl:start(),
    loop().

loop() ->
    receive
	{sendmail, Email1, Sex1, Email2, Sex2}->
	    sendMail(Email1, Sex1, Email2, Sex2),
	    loop();
	_Other ->
	    loop()
    end.

sendMail(Email1, Sex1, Email2, Sex2) ->
    {ok, Socket} = ssl:connect("smtp.gmail.com", 465, [{active, false}], 1000),
    recv(Socket),
    send(Socket, "HELO localhost"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode("rmxyz.notify@gmail.com"))),
    send(Socket, binary_to_list(base64:encode("123456rmxyz"))),
    send(Socket, "MAIL FROM: <rmxyz.notify@gmail.com>"),
    send(Socket, ["RCPT To: <", Email1,">"]),
    send(Socket, ["RCPT To: <", Email2,">"]),    
    send(Socket, "DATA"),
    send_no_receive(Socket, "From: <nov503@gmail.com>"),
    send_no_receive(Socket, ["To: <", Email1,">,<", Email2, ">"]),
    send_no_receive(Socket, "Subject: rmxyz-Matched!"),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, ["Congratulations! You two have made a mutual choice in an unforgettable chat, please allow me to introduce you to each other:\n\r", Email1, ",", Sex1, " \n\r", Email2, "," , Sex2, "\n\r", "To me, it is kind of destiny because you have both accessed the same website, at the same time ,in the same room and made a choice each other! Sincerely, I wish you can have a life-long friendship from now on. \n\rEnjoy!"]),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    ssl:close(Socket).

send_no_receive(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n").


send(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

recv(Socket) ->
    case ssl:recv(Socket, 0, 1000) of
	{ok, Return} -> io:format("~p~n", [Return]);
	{error, Reason} -> io:format("ERROR: ~p~n", [Reason])
    end.
