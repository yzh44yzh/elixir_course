# Supervisor

## Зачем нужен супервизор

Мы знаем, что одни процессы могут отслеживать падение других процессов через механизмы link и monitor. Логичный шаг -- поручить части процессов в системе заниматься именно этим. 

И таким образом все процессы поделятся на две категории:
- worker -- процесс, выполняющий всю полезную работу;
- supervisor -- процесс обрабатывающий падение worker.

На самом деле супервизор не просто занимается падениями процессов, а обеспечивает их полный жизненный цикл:
- Start -- запуск;
- Restart -- перезапуск;
- Shutdown -- завершение, штатное и аварийное.


## Start

Супервизор при запуске получает **child specification**. Это структура данных, которая декларативно описывает, какие дочерние процессы должны быть запущены и каким именно образом. Выглядит она так:

```
[
  %{
    id: "child_1",
    start: {PathFinder, start_link, []},
    restart: :permanent,
    shutdown: 5000,
    type: :worker
  },
  %{
    id: "child_2",
    start: {Lesson_10.Task_01_Map_Reduce, start, [processes_tree]}
  },
  %{
    id: "child_3",
    start: {Lesson_10.Task_02_Sharding, start, []}
  }
]
```

Здесь мы указываем 3 дочерних процесса и описываем их в виде Map с определенным набором ключей:

**:id** -- идентификатор процесса. Мы привыкли идентифицировать процессы по pid, но в данном случае это не подходит, потому что процесс может несколько раз запускаться и завершаться, при этом pid будет меняться. Поэтому супервизор использует отдельный идентификатор, который в каждый конкретный момент времени указывает на один конкретный процесс, но в разное время это могут быть разные процессы.

**:start** -- кортеж `{Module, function, args}`, который описывает, как запустить процесс. (Такой кортеж обычно называют MFA).

**:restart** -- стратегия рестарта (будет описана ниже).

**:shutdown** -- стратегия завершения процесса (будет описана ниже).

**:type** -- тип дочернего процесса: `:worker` или `:supervisor`.

Первые два ключа обязательны, для остальных супервизор подставит значения по-умолчанию, если они не будут указаны явно.

Получив child specification, супервизор запускает дочерние процессы по очереди. Для каждого элемента списка супервизор вызвает MFA и блокируется, пока не получит ответ, затем переходит к следущему элементу. В случае с GenServer супервизор блокируется, пока не завершится init. Поэтому важно, чтобы init не выполнялся слишком долго.


## Restart

Есть две настройки, которые определяют, как супервизор рестартует процессы. Это ключ `:restart` в child specification и ключ `:strategy` в настройках самого супервизора.

**:restart** относится к дочернему процессу и имеет 3 варианта:
- :permanent - процесс нужно перезапускать всегда;
- :temporary - процесс не нужно перезапускать;
- :transient - процесс нужно перезапустить, если он завершился аварийно, и не нужно перезапускать, если он завершился в штатном режиме.

Тут важно разобраться, что такое штатное и аварийное завершение.

Как мы помним, когда процесс завершается по любой причине, отправляется сигнал `exit` всем другим процессам, связанным link. Разумеется, супервизор связывается со своими дочерними процессами, и устанавливает у себя флаг trap_exit:
```
Process.flag(:trap_exit, true)
```
так что эти сигналы попадают к нему в почтовый ящик в виде сообщений:
```
{:EXIT, #PID<0.122.0>, reason}
```

reason -- это причина завершения процесса и здесь может быть любая структура данных. 

Супервизор считает, что процесс завершился штатно, если reason:
- :normal
- :shutdown
- {:shutdown, any}

Любые другие значения считаются не штатным (аварийным) завершением процесса.



Restarting boils down to starting another process in place of the old one. 
The new process has a different pid and doesn't share any state with the old one.
Any reference to the pid of the crashed process becomes invalid.

RestartStrategy описывает политику перезапуска дочерних потоков.
Есть 4 варианта стратегии:

**one_for_one** -- при падении одного потока перезапускается только
этот поток, остальные продолжают работать.

**one_for_all** -- при падении одного потока перезапускаются все
дочерние потоки.

**rest_for_one** -- промежуточный вариант между двумя первыми
стратегиями. Суть в том, что изначально потоки запущены один за одним,
в определенной последовательности. И при падении одного потока,
перезапускается он, и те потоки, которые были запущены позже него. Те,
которые были запущены раньше, продолжают работать.

**simple_one_for_one** -- это особый вариант, будет рассмотрен ниже.

Многие проблемы можно решить рестартом, но не все. Супервизор должен
как-то справляться с ситуацией, когда рестарт не помогает.  Для этого
есть еще две настройки: **Intensity** -- максимальное количество
рестартов, и **Period** -- за промежуток времени.

Например, если Intensity = 10, а Period = 1000, это значит, что
разрешено не более 10 рестартов за 1000 миллисекунд. Если поток падает
11-й раз, то супервизор понимает, что он не может исправить проблему.
Тогда супервизор завершается сам, а проблему пытается решить его
родитель -- супервизор уровнем выше.

Есть смысл запускать воркера под супервизором даже если он :temporary, и его не нужно рестартовать.
В этом случае воркер не зависнет в системе при крашах и рестартах, а гарантировано завершится.
И краш в этом воркере будет правильно логирован.


## Shutdown

When a supervisor shuts down, it terminates all children in the opposite order they are listed. The termination happens by sending a shutdown exit signal, via Process.exit(child_pid, :shutdown), to the child process and then awaiting for a time interval for the child process to terminate. 
defaults to 5000 milliseconds.
If the child process does not terminate in this interval, the supervisor abruptly terminates the child with reason :kill.

If the child process is not trapping exits, it will shutdown immediately when it receives the first exit signal. If the child process is trapping exits, then the terminate callback is invoked, and the child process must terminate in a reasonable time interval before being abruptly terminated by the supervisor.

In other words, if it is important that a process cleans after itself when your application or the supervision tree is shutting down, then this process must trap exits and its child specification should specify the proper :shutdown value, ensuring it terminates within a reasonable interval.

Note that the supervisor that reaches maximum restart intensity will exit with :shutdown reason.



Exit Reason:

:normal - in such cases, the exit won't be logged, there is no restart in transient mode, and linked processes do not exit

:shutdown or {:shutdown, term} - in such cases, the exit won't be logged, there is no restart in transient mode, and linked processes exit with the same reason unless they're trapping exits

any other term - in such cases, the exit will be logged, there are restarts in transient mode, and linked processes exit with the same reason unless they're trapping exits

The following shutdown values are supported in the :shutdown option:

:brutal_kill - the child process is unconditionally and immediately terminated using Process.exit(child, :kill).

any integer >= 0 - the amount of time in milliseconds that the supervisor will wait for its children to terminate after emitting a Process.exit(child, :shutdown) signal. If the child process is not trapping exits, the initial :shutdown signal will terminate the child process immediately. If the child process is trapping exits, it has the given amount of time to terminate. If it doesn't terminate within the specified time, the child process is unconditionally terminated by the supervisor via Process.exit(child, :kill).

:infinity - works as an integer except the supervisor will wait indefinitely for the child to terminate. If the child process is a supervisor, the recommended value is :infinity to give the supervisor and its children enough time to shut down. This option can be used with regular workers but doing so is discouraged and requires extreme care. If not used carefully, the child process will never terminate, preventing your application from terminating as well.

defaults to 5_000 if the type is :worker or :infinity if the type is :supervisor.


## Дерево супервизоров (Supervision Tree)

![supervision_tree](http://yzh44yzh.github.io/img/practical_erlang/supervision_tree.png)

На картинке нарисовано такое дерево. Узлы в нем -- супервизоры, а
листья -- рабочие процессы.  Падение любого потока и любой части
системы не останется незамеченным.


a nested structure of supervisors and workers.
The tree describes how the system is organized into a hierarchy of services.
The tree describes how the system is started and how it's taken down.
A more granular tree allows you to take down part of the system.

You try to recover from an error locally. If that doesn't work, you move up and try to restart the wider part of the system.

Graceful termination of a GenServer worker involves invoking the terminate/2 callback, but only if the worker process is trapping exits. 
TODO Вот этого я не понимаю. Почему так сделано? 
