-module(nat_pmp).
-export([start/0,init/0]).
-export([udp_mapping/3,tcp_mapping/3]).

start() ->
    spawn(nat_pmp,init,[]).

udp_mapping(Pid,InternalPort,ExternalPort) ->
    Pid ! {udp_port_mapping,InternalPort,ExternalPort},
    ok.

tcp_mapping(Pid,InternalPort,ExternalPort) ->
    Pid ! {tcp_port_mapping,InternalPort,ExternalPort},
    ok.

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
    loop({Socket,Ip}).

loop({Socket,Ip}) ->
    receive
        {udp,Socket,_Ip,_Port,Data} ->
            parse(Data),
            loop({Socket,Ip});

        {udp_port_mapping,InternalPort,ExternalPort} ->
            gen_udp:send(Socket,Ip,5351,<<0,1,0:16,InternalPort:16,ExternalPort:16,7200:32>>),
            loop({Socket,Ip});

        {tcp_port_mapping,InternalPort,ExternalPort} ->
            gen_udp:send(Socket,Ip,5351,<<0,2,0:16,InternalPort:16,ExternalPort:16,7200:32>>),
            loop({Socket,Ip})
    end.

parse(<<0,128,_Result:16,Seconds:32,A,B,C,D>>) ->
    io:format("Time since last restart: ~bs~n",[Seconds]),
    io:format("Public IP is ~s~n",[inet:ntoa({A,B,C,D})]);

parse(<<0,129,0:16,_Seconds:32,InternalPort:16,ExternalPort:16,LifeTime:32>>) ->
    io:format("Successfully created udp mapping ~b <> ~b, expires in ~bs~n",[InternalPort,ExternalPort,LifeTime]);

parse(<<0,130,0:16,_Seconds:32,InternalPort:16,ExternalPort:16,LifeTime:32>>) ->
    io:format("Successfully created tcp mapping ~b <> ~b, expires in ~bs~n",[InternalPort,ExternalPort,LifeTime]).
