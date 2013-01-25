%#!/usr/bin/env escript

-module(flow).
-compile([export_all]).

handler() ->
    receive
	M ->
	    io:format("handler: ~p~n", [M]) 
    end,
    handler().

read(Filename) ->
    {ok, B} = file:read_file(Filename),
    Handler = spawn_link(fun handler/0),
    nmf:data(B, Handler).
