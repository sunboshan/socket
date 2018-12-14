# Socket

Play with Erlang socket.

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
