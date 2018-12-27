-module(upnp).
-export([start/0,init/0]).

start() ->
    spawn(upnp,init,[]).

init() ->
    {ok,Socket}=gen_udp:open(0,[binary,{active,true}]),
    MSearch = <<"M-SEARCH * HTTP/1.1\r\n"
                "HOST: 239.255.255.250:1900\r\n"
                "MAN: \"ssdp:discover\"\r\n"
                "ST: urn:schemas-upnp-org:device:InternetGatewayDevice:1\r\n"
                "MX: 3\r\n\r\n">>,
    gen_udp:send(Socket, "239.255.255.250", 1900, MSearch),
    loop(Socket).

loop(Socket) ->
    receive
        {udp,Socket,_Ip,_Port,Data} ->
            io:format("~s~n",[Data]),
            loop(Socket)
    end.
