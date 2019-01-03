-module(rpc1).
-export([call/3]).
-export([start/0,init/0]).

call(M,F,A) ->
    {ok,Socket}=gen_udp:open(0,[binary,{active,true}]),
    Bin=erlang:term_to_binary({M,F,A}),
    gen_udp:send(Socket,{127,0,0,1},9527,<<"rpc",Bin/binary>>),
    receive
        {udp,Socket,{127,0,0,1},9527,<<"res",Bin0/binary>>} ->
            erlang:binary_to_term(Bin0)
    after 1000 -> timeout
    end.

start() ->
    spawn(rpc1,init,[]).

init() ->
    {ok,Socket}=gen_udp:open(9527,[binary,{active,true}]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp,Socket,Ip,Port,<<"rpc",Bin/binary>>} ->
            {M,F,A}=erlang:binary_to_term(Bin),
            Res=apply(M,F,A),
            Bin1=erlang:term_to_binary(Res),
            gen_udp:send(Socket,Ip,Port,<<"res",Bin1/binary>>),
            loop(Socket)
    end.
