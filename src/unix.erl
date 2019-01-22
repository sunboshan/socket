-module(unix).
-export([start/1,init/1]).
-export([send/3]).

start(File) ->
    spawn(unix,init,[File]).

send(Pid,File,Msg) ->
    Pid ! {send,File,Msg},
    ok.

init(File) ->
    {ok,Socket}=gen_udp:open(0,[binary,{active,true},{ifaddr,{local,File}}]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp,_Socket,{local,File},0,Msg} ->
            io:format("~s ~ts~n",[File,Msg]),
            loop(Socket);

        {send,File,Msg} ->
            gen_udp:send(Socket,{local,File},0,Msg),
            loop(Socket)
    end.

