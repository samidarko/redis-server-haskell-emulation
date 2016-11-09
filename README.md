# REDIS SERVER EMULATION IN HASKELL

## What is this repository?

This a an emulation of a [Redis][1] server, once it is running you can connect to it with a Redis client and interact with it.

If you need to use this protocol in Haskell you should look at [redis-resp][4] repository which a lot better implementation. It's just a library, you wont have the server.

## How does it work?

I implemented the [Redis protocol][3], I'm communicating with the redis client via the port socket *6388*

## How to compile it

It's built on top of [Haskell stack][2]

```bash
stack build
```

## How to use it?

Execute: 

```bash
stack runghc app/Main.hs
```

## Screenshot

<img src="https://github.com/samidarko/redis-server-haskell-emulation/blob/master/screenshot.png" alt="alt text" width="600" height="400">

## TODO

 * Write more tests
 * Implements more commands
 
[1]: http://redis.io
[2]: https://docs.haskellstack.org/en/stable/README/
[3]: http://redis.io/topics/protocol
[4]: https://gitlab.com/twittner/redis-resp
