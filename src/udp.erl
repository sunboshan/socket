-module(udp).
-export([start/1,init/1]).
-export([send/3]).

start(Port) ->
    spawn(udp,init,[Port]).

send(Pid,Port,Msg) ->
    Pid ! {send,Port,Msg},
    ok.

init(Port) ->
    {ok,Socket}=gen_udp:open(Port,[binary,{active,true}]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp,_Socket,Ip,Port,Msg} ->
            io:format("~w:~w ~ts~n",[Ip,Port,Msg]),
            loop(Socket);

        {send,Port,Msg} ->
            gen_udp:send(Socket,{127,0,0,1},Port,Msg),
            loop(Socket)
    end.
