-module(echo).
-export([start/1,init/1,acceptor/1]).

start(Port) ->
    spawn(echo,init,[Port]).

init(Port) ->
    {ok,LSocket}=gen_tcp:listen(Port,[binary]),
    spawn(echo,acceptor,[LSocket]),
    timer:sleep(infinity).

acceptor(LSocket) ->
    {ok,Socket}=gen_tcp:accept(LSocket),
    {ok,{Ip,Port}=Peer}=inet:peername(Socket),
    io:format("~s:~b connected!~n",[inet:ntoa(Ip),Port]),
    spawn(echo,acceptor,[LSocket]),
    loop(Socket,Peer).

loop(Socket,Peer) ->
    receive
        {tcp,Socket,Msg} ->
            gen_tcp:send(Socket,Msg),
            loop(Socket,Peer);
        {tcp_closed,Socket} ->
            {Ip,Port}=Peer,
            io:format("~s:~b disconnected!~n",[inet:ntoa(Ip),Port])
    end.
