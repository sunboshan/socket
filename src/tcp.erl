-module(tcp).
-export([start/1,start_client/1,send/2]).
-export([init/1,init_client/1,acceptor/1]).

start(Port) ->
    spawn(tcp,init,[Port]).

start_client(Port) ->
    spawn(tcp,init_client,[Port]).

send(Pid,Msg) ->
    Pid ! {send,Msg},
    ok.

init(Port) ->
    {ok,ListenSocket}=gen_tcp:listen(Port,[binary,{active,true}]),
    spawn(tcp,acceptor,[ListenSocket]),
    timer:sleep(infinity).

init_client(Port) ->
    {ok,Socket}=gen_tcp:connect({127,0,0,1},Port,[binary,{active,true}]),
    loop(Socket).

acceptor(ListenSocket) ->
    {ok,Socket}=gen_tcp:accept(ListenSocket),  % waiting for the connection
    io:format("Socket ~w connected.~n",[Socket]),
    spawn(tcp,acceptor,[ListenSocket]),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp,Socket,Msg} ->
            io:format("~w: ~ts~n",[Socket,Msg]),
            loop(Socket);

        {tcp_closed,Socket} ->
            io:format("Socket ~w disconnected.~n",[Socket]);

        {send,Msg} ->
            gen_tcp:send(Socket,Msg),
            loop(Socket);

        Msg ->
            io:format("Got Msg ~p~n",[Msg])
    end.
