# Socket

Play with Erlang socket.

## echo

![echo](../assets/echo.gif?raw=true)

A simple echo server using tcp. Open up two shells.

```
$ make run                       |
1> echo:start(9527).             |
<0.84.0>                         |
                                 |        $ telnet 127.0.0.1 9527
Socket #Port<0.7> connected.     |        Trying 127.0.0.1...
                                 |        Connected to localhost.
                                 |        Escape character is '^]'.
                                 |        123
                                 |        123
                                 |        abc
                                 |        abc
                                 |        ^]
                                 |        telnet> quit
Socket #Port<0.7> disconnected.  |        Connection closed.
```

## udp

Open up two shells.

```
$ make run                       |        $ make run
1> P=udp:start(9527).            |        1> P=udp:start(9999).
<0.84.0>                         |        <0.84.0>
2> udp:send(P,9999,"hello").     |
ok                               |        127.0.0.1:9527 hello
                                 |        2> udp:send(P,9527,"hi").
127.0.0.1:9999 hi                |        ok
```

## tcp

Open up two shells.

```
$ make run                       |        $ make run
1> tcp:start(9527).              |
<0.84.0>                         |        1> P=tcp:start_client(9527).
Socket #Port<0.7> connected.     |        <0.84.0>
                                 |        2> tcp:send(P,"hi").
#Port<0.7>: hi                   |        ok
Socket #Port<0.7> disconnected.  |        % ctrl+\ to exit
                                 |
                                 |        $ telnet 127.0.0.1 9527
Socket #Port<0.8> connected.     |        Trying 127.0.0.1...
                                 |        Connected to localhost.
                                 |        Escape character is '^]'.
                                 |        123
#Port<0.8>: 123                  |
                                 |        abc
#Port<0.8>: abc                  |
                                 |        telnet> Connection closed.
Socket #Port<0.8> disconnected.  |
```

## nat_pmp

Simple NAT-PMP implementation. [RFC 6886](https://tools.ietf.org/html/rfc6886).

```
$ make run
1> P=nat_pmp:start().
<0.103.0>
Time since last restart: 5525s
Public IP is 238.88.166.250
2> nat_pmp:tcp_mapping(P,6677,56789).
ok
Successfully created tcp mapping 6677 <> 56789, expires in 7200s
```

Now open up two shells, one from local, one from anywhere.

```
$ make run                       |        $ erl  % from anywhere
1> tcp:start(6677).              |
<0.84.0>                         |        1> {ok,S}=gen_tcp:connect({238,88,166,250},56789,[]).
Socket #Port<0.7> connected.     |        {ok,#Port<0.6>}
                                 |        2> gen_tcp:send(S,"hi").
#Port<0.7>: hi                   |        ok
Socket #Port<0.7> disconnected.  |        % ctrl+\ to exit
```

This indicates a localhost ip `127.0.0.1:6677` is talking to a remote ip via NAT gateway `238.88.166.250:56789`.
