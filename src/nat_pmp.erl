-module(nat_pmp).
-export([start/0,init/0]).
-export([udp_mapping/3,tcp_mapping/3,get_internal_ip/0]).

start() ->
    spawn(nat_pmp,init,[]).

udp_mapping(Pid,InternalPort,ExternalPort) ->
    Pid ! {{udp_port_mapping,InternalPort,ExternalPort},self()},
    receive
        {udp_reply,Addr} -> {ok,Addr}
    after
        1000 -> {error,timeout}
    end.

tcp_mapping(Pid,InternalPort,ExternalPort) ->
    Pid ! {tcp_port_mapping,InternalPort,ExternalPort},
    ok.

get_internal_ip() ->
    {ok,Ifaddrs}=inet:getifaddrs(),
    Ip=hd([Addr || {_,Opts}<-Ifaddrs,{addr,Addr}<-Opts,size(Addr)==4,Addr=/={127,0,0,1}]),
    inet:ntoa(Ip).

init() ->
    {ok,Ip}=
        case os:type() of
            {unix,darwin} ->
                Ip0=os:cmd("ipconfig getoption en0 router"),
                Ip1=string:strip(Ip0,right,$\n),
                inet:parse_address(Ip1)
        end,
    put(internal_ip,get_internal_ip()),
    {ok,Socket}=gen_udp:open(0,[binary,{active,true}]),
    gen_udp:send(Socket,Ip,5351,<<0,0>>),
    loop({Socket,Ip}).

loop({Socket,Ip}) ->
    receive
        {udp,Socket,Ip,_Port,Data} ->
            parse(Data),
            loop({Socket,Ip});

        {{udp_port_mapping,InternalPort,ExternalPort},From} ->
            gen_udp:send(Socket,Ip,5351,<<0,1,0:16,InternalPort:16,ExternalPort:16,7200:32>>),
            put(from,From),
            loop({Socket,Ip});

        {tcp_port_mapping,InternalPort,ExternalPort} ->
            gen_udp:send(Socket,Ip,5351,<<0,2,0:16,InternalPort:16,ExternalPort:16,7200:32>>),
            loop({Socket,Ip})
    end.

parse(<<0,128,_Result:16,Seconds:32,A,B,C,D>>) ->
    ExternalIp=inet:ntoa({A,B,C,D}),
    put(external_ip,ExternalIp),
    io:format("Time since last restart: ~bs~n",[Seconds]),
    io:format("Public IP is ~s~n",[ExternalIp]);

parse(<<0,129,0:16,_Seconds:32,InternalPort:16,ExternalPort:16,LifeTime:32>>) ->
    io:format("Successfully created udp mapping ~s:~b <> ~s:~b, expires in ~bs~n",[
      get(internal_ip),InternalPort,get(external_ip),ExternalPort,LifeTime]),
    From=get(from),
    From ! {udp_reply,{get(external_ip),ExternalPort}};

parse(<<0,130,0:16,_Seconds:32,InternalPort:16,ExternalPort:16,LifeTime:32>>) ->
    io:format("Successfully created tcp mapping ~s:~b <> ~s:~b, expires in ~bs~n",[
      get(internal_ip),InternalPort,get(external_ip),ExternalPort,LifeTime]).
