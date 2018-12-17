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
    io:format("~w connected!~n",[Socket]),
    spawn(echo,acceptor,[LSocket]),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp,Socket,Msg} ->
            gen_tcp:send(Socket,Msg),
            loop(Socket);
        {tcp_closed,Socket} ->
            io:format("~w disconnected!~n",[Socket])
    end.
