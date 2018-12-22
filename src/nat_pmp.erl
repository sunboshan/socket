-module(nat_pmp).
-export([start/0,init/0]).

start() ->
    spawn(nat_pmp,init,[]).

init() ->
    {ok,Socket}=gen_udp:open(0,[binary,{active,true}]),
    gen_udp:send(Socket,{10,0,1,1},5351,<<0,0>>),
    loop(Socket).

loop(Socket) ->
    receive
        {udp,Socket,Ip,Port,Data} ->
            parse(Data),
            loop(Socket)
    end.

parse(<<0,128,_Result:16,Seconds:32,A,B,C,D>>) ->
    io:format("Seconds: ~b~n",[Seconds]),
    io:format("Public IP is ~s~n",[inet:ntoa({A,B,C,D})]).
