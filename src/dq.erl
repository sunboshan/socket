-module(dq).
-export([main/1]).
-export([start/0,init/1]).

-define(NAT_RENEW_PERIOD,3600000).

main([]) ->
    case os:getenv("PORT") of
        false ->
            start();

        Port ->
            os:putenv("BOOT","38.88.166.250:"++Port),
            start()
    end.

start() ->
    Pid=spawn(dq,init,[os:getenv("BOOT")]),
    io_loop(Pid).

io_loop(Pid) ->
    Msg=string:strip(io:get_line("> "),right,$\n),
    Pid ! {send,Msg},
    io_loop(Pid).

init(false) ->
    % bootstrap node
    {ok,Socket}=gen_udp:open(0,[binary,{active,true}]),
    {ok,Port}=inet:port(Socket),
    Pid=nat_pmp:start(),
    {ok,{_EIp,EPort}=Addr}=nat_pmp:udp_mapping(Pid,Port,9527),
    erlang:send_after(?NAT_RENEW_PERIOD,self(),{renew_nat,Port,EPort}),
    LocalAddr={nat_pmp:get_internal_ip(),Port},
    loop(#{addr => Addr,local_addr => LocalAddr,nat => Pid,peers=>[],socket=>Socket});

init(Addr0) ->
    % non bootstrap node
    {ok,Socket}=gen_udp:open(0,[binary,{active,true}]),
    {ok,Port}=inet:port(Socket),
    [Ip0,Port0]=string:split(Addr0,":"),
    gen_udp:send(Socket,Ip0,list_to_integer(Port0),<<"init">>),
    LocalAddr={nat_pmp:get_internal_ip(),Port},
    loop(#{local_addr => LocalAddr,socket=>Socket}).

loop(State) ->
    receive
        get ->
            io:format("~p~n",[State]),
            loop(State);

        {send,Msg} ->
            #{addr := {Ip,Port},peers := Peers,socket := Socket}=State,
            io:format("~s:~b ~s~n",[Ip,Port,Msg]),
            MsgRef=make_ref(),
            [gen_udp:send(Socket,Ip1,Port1,["msg",erlang:term_to_binary({Msg,MsgRef})]) || {Ip1,Port1} <- Peers],
            loop(State#{msg_ref => MsgRef});

        {renew_nat,Port,EPort}=Msg ->
            erlang:send_after(?NAT_RENEW_PERIOD,self(),Msg),
            #{nat := Pid}=State,
            {ok,_Addr}=nat_pmp:udp_mapping(Pid,Port,EPort),
            loop(State);

        {udp,Socket,Ip,Port,<<"init">>} ->
            io:format("~s:~b joined the network~n",[inet:ntoa(Ip),Port]),
            #{peers := Peers}=State,
            gen_udp:send(Socket,Ip,Port,["init_ack",<<Port:16>>,"peers",erlang:term_to_binary(Peers)]),
            loop(State#{peers := [{Ip,Port}|Peers]});

        {udp,Socket,Ip0,Port0,<<"init_ack",EPort:16,"peers",Bin/binary>>} ->
            % nat my own port
            {ok,Port}=inet:port(Socket),
            Pid=nat_pmp:start(),
            {ok,{_EIp,EPort}=Addr}=nat_pmp:udp_mapping(Pid,Port,EPort),
            erlang:send_after(?NAT_RENEW_PERIOD,self(),{renew_nat,Port,EPort}),

            % add and notify peers
            Peers=erlang:binary_to_term(Bin),
            [gen_udp:send(Socket,Ip1,Port1,<<"ping">>) || {Ip1,Port1} <- Peers],

            loop(State#{addr => Addr,nat => Pid,peers => [{Ip0,Port0}|Peers]});

        {udp,_Socket,Ip,Port,<<"ping">>} ->
            io:format("~s:~b joined the network~n",[inet:ntoa(Ip),Port]),
            #{peers := Peers}=State,
            loop(State#{peers := [{Ip,Port}|Peers]});

        {udp,_Socket,Ip,Port,<<"msg",Bin/binary>>} ->
            {Msg,MsgRef}=erlang:binary_to_term(Bin),
            case maps:find(msg_ref,State) of
                {ok,MsgRef} ->
                    % already print the message
                    ok;
                _ ->
                    % first time see the message
                    io:format("~s:~b ~s~n",[inet:ntoa(Ip),Port,Msg])
            end,
            loop(State#{msg_ref => MsgRef});

        {udp,_Socket,_Ip,_Port,_Data}=Msg ->
            io:format("~p~n",[Msg]),
            loop(State)
    end.
