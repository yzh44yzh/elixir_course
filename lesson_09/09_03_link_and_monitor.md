Kernel.spawn_link/1 and Kernel.spawn_link/3
Kernel.spawn_monitor/1 and Kernel.spawn_monitor/3

Process.monitor demonitor
link unlink

A basic primitive for detecting a process crash is the concept of links.

When processes are linked, each can receive information when the other exits (exit signal).

Exit signal contains:
- the pid of the crashed process;
- exit reason (arbitrary term).

In case of normal termination the exit reason is the atom :normal.
If exit reason is anything other than :normal, the linked process is also taken down. 

So our child process died, and it killed the entire application. 
That’s the default behavior of linked processes — when one exits abnormally, it kills the other.

However, you can tell Elixir to convert the exit signals from a linked process into a message you can handle. 
Do this by trapping the exit.

If a process is trapping exits, it isn't taken down. Instead, an exit signal is turned into usual message and is placed into message queue. A trapping process can receive these messages and do something about the crash.

```
Process.flag(:trap_exit, true)
{:EXIT, from_pid, exit_reason}
```


## Monitor

By contrast, monitoring lets a process spawn another and be notified of its termination, 
but without the reverse notification — it is one-way only.

You can use spawn_monitor to turn on monitoring when you spawn a process,
or you can use Process.monitor to monitor an existing process.

The spawn_link and spawn_monitor versions are atomic, so you’ll always catch a failure.

Devin Torres reminded me that every book in the Erlang space must, by law, include a parallel map function.

```
monitor_ref = Process.monitor(target_pid)
{:DOWN, monitor_ref, :process, from_pid, exit_reason}
```

GenServer.call uses temporary monitor.

