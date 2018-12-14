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
