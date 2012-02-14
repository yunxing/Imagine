-module(web_server).

-export([start/0]).

start()->
    io:format("web server started"),
    application:load(imagine),
    application:start(imagine).

