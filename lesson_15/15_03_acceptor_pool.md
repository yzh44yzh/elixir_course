# Сервер с Acceptor Pool

- GenServer для listener process 
- Запустить под супервизором сразу.
- GenServer дял acceptor processes.
- Запустить под супервизором из Listener, чтобы передать listening socket
  или так 
  https://github.com/yzh44yzh/practical_erlang/blob/master/16_sockets/solution/src/mcache_server.erl

TODO В чём преимущество пула?

TODO: на базе acceptor pool реализовать варианты:
- [X] text protocol, active:true
- [X] text protocol, active:false


## reuseaddr

Если сервис завершается аварийно, то ОС не сразу освобождает порт. Из-за этого при рестарте сервис не может занять свой порт.

```
** (EXIT from #PID<0.128.0>) shell process exited with reason: shutdown: failed to start child: Server.Listener
    ** (EXIT) an exception was raised:
        ** (MatchError) no match of right hand side value: {:error, :eaddrinuse}
            acceptor_pool.exs:79: Server.Listener.init/1
            (stdlib 4.0.1) gen_server.erl:848: :gen_server.init_it/2
            (stdlib 4.0.1) gen_server.erl:811: :gen_server.init_it/6
            (stdlib 4.0.1) proc_lib.erl:240: :proc_lib.init_p_do_apply/3
```

Проблема решается настройкой reuseaddr:

```
  options = [
    :binary,
    {:active, true},
    {:reuseaddr, true}
  ]
  {:ok, listening_socket} = :gen_tcp.listen(port, options)
```
