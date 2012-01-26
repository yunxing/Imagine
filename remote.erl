-module(remote).
-compile([export_all]).

rpc(Process, Req)->
    Process ! {self(),Req},
    receive
	R -> R
    end.
