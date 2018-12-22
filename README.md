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
ok                               |        {127,0,0,1}:9527 hello
                                 |        2> udp:send(P,9527,"hi").
{127,0,0,1}:9999 hi              |        ok
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
1> nat_pmp:start().
<0.103.0>
Seconds: 9537
Public IP is 38.88.166.250
```