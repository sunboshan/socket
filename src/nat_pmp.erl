-module(nat_pmp).
-export([start/0,init/0]).

start() ->
    spawn(nat_pmp,init,[]).

init() ->
    {ok,Ip}=
        case os:type() of
            {unix,darwin} ->
                Ip0=os:cmd("ipconfig getoption en0 router"),
                Ip1=string:strip(Ip0,right,$\n),
                inet:parse_address(Ip1)
        end,
    {ok,Socket}=gen_udp:open(0,[binary,{active,true}]),
    gen_udp:send(Socket,Ip,5351,<<0,0>>),
    loop(Socket).

loop(Socket) ->
    receive
        {udp,Socket,_Ip,_Port,Data} ->
            parse(Data),
            loop(Socket)
    end.

parse(<<0,128,_Result:16,Seconds:32,A,B,C,D>>) ->
    io:format("Seconds: ~b~n",[Seconds]),
    io:format("Public IP is ~s~n",[inet:ntoa({A,B,C,D})]).
