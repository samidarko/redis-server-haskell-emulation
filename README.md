# REDIS SERVER EMULATION IN HASKELL

## What is this repository?

This a an emulation of a [Redis][1] server, once it is running you can connect to it with a Redis client and interact with it.

## How does it work?

I implemented the [Redis protocol][3], I'm communicating with the redis client via the port socket *6388*

## How to compile it

It's built on top of [Haskell stack][2]

```bash
stack build
```

## How to use it?

Call the compiled binary from the command line or just 

```bash
stack runghc app/Main.hs
```

[1]: http://redis.io
[2]: https://docs.haskellstack.org/en/stable/README/
[3]: http://redis.io/topics/protocol
