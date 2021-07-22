## Мониторинг процессов

Кроме link, есть еще один механизм наблюдения за состоянием процессов -- монитор.

Этот механизм существенно отличается:
- связь односторонняя;
- информация сразу приходит в виде сообщения, а не в виде сигнала;
- мониторов может быть несколько, и каждый работает независимо от остальных;
- получателю не нужно быть системным процессом;
- информация о нормальном завершении тоже доходит и обрабатывается.

Текущий процесс может установить монитор над другим процессом вызовом:
``` TODO elixir
Reference = erlang:monitor(process, Pid)
```

В аргументах передаются атом _process_ и Pid процесса, который нужно мониторить.  Возвращается ссылка на установленный монитор. (Первый аргумент предполагает, что мониторить можно что-то еще, кроме процессов. Но подробности смотрите в документации).

Если процесс завершается, то второй процесс получает сообщение:
``` TODO elixir
{'DOWN', Reference, process, Pid, Reason}
```

Где Reference -- это ссылка на монитор, Pid -- завершившийся процесс, Reason -- причина завершения процесса.

Установленный монитор можно снять:
``` TODO elixir
erlang:demonitor(Reference, [flush]).
```

Опция flush удаляет из почтового ящика сообщение вида {'DOWN', Reference, process, Pid, Reason}, если оно есть.


## Заключение

Использовать низкоуровневые функции **link/unlink** и создавать свои системные процессы не рекомендуется. Нужен хороший опыт, чтобы грамотно пользоваться этими средствами.  К счастью, это редко бывает нужно, потому что у нас есть высокоуровневое средство -- супервизор.

Супервизор построен поверх **link** и **trap_exit**, построен хорошо и отлажен годами использования в нагруженных проектах.  Вот его и нужно использовать. Это тема следующего урока.

**monitor** используется чаще, когда нам не хватает тех вариантов обработки, которые предлагает супервизор.


------------------------------------------------

Kernel.spawn_link/1 and Kernel.spawn_link/3
Kernel.spawn_monitor/1 and Kernel.spawn_monitor/3

Process.monitor demonitor


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

