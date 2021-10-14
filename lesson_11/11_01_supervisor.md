# Supervisor

## Зачем нужен супервизор

Links, trap exits and monitors make it possible to detect errors in concurrent system.
You can introduce a process whose only responsibility is to receive links and monitor notifications,
and do something when a process crashed.
Such processes, called Supervisors, are the primary tools of error recovery in concurrent systems.

Think in terms of Lifecycles, not in terms of Supervision.
start, stop, restart -- it is a lifecycle.

A supervisor is a generic process that manages the lifecycle of other processes.
- starts other processes (which are then considered to be its children);
- detects termination of any child;
- restart it if needed.

A supervisor is a process which supervises other processes, which we refer to as child processes. Supervisors are used to build a hierarchical process structure called a supervision tree

The act of supervising a process includes three distinct responsibilities. 

The first one is to start child processes. 

Once a child process is running, the supervisor may restart a child process, either because it terminated abnormally or because a certain condition was reached. For example, a supervisor may restart all children if any child dies. 

Finally, a supervisor is also responsible for shutting down the child processes when the system is shutting down.


## Запуск

After the supervisor retrieves all child specifications, it proceeds to start its children one by one, in the order they were defined, using the information in the :start key in the child specification.

When the supervisor starts, it traverses all child specifications and then starts each child in the order they are defined. This is done by calling the function defined under the :start key in the child specification and typically defaults to start_link/1.

The start_link/1 (or a custom) is then called for each child process. The start_link/1 function must return {:ok, pid} where pid is the process identifier of a new process that is linked to the supervisor. The child process usually starts its work by executing the init/1 callback. Generally speaking, the init callback is where we initialize and configure the child process.

Children are started synchronously, in the order specified. The next child is started only after the init/1 for the current child is finished. That's why init/1 shouldn't run for a long time.


## Child Specification

The child specification describes how the supervisor starts, shuts down, and restarts child processes.

:id - any term used to identify the child specification internally by the supervisor; defaults to the given module. In the case of conflicting :id values, the supervisor will refuse to initialize and require explicit IDs. This key is required.

:start - a tuple with the module-function-args to be invoked to start the child process. This key is required.

:restart - an atom that defines when a terminated child process should be restarted (see the "Restart values" section below). This key is optional and defaults to :permanent.

:shutdown - an integer or atom that defines how a child process should be terminated (see the "Shutdown values" section below). This key is optional and defaults to 5_000 if the type is :worker or :infinity if the type is :supervisor.

:type - specifies that the child process is a :worker or a :supervisor. This key is optional and defaults to :worker.


```
child_spec() :: %{
  :id => atom() | term(),
  :start => {module(), atom(), [term()]},
  optional(:restart) => :permanent | :transient | :temporary,
  optional(:shutdown) => timeout() | :brutal_kill,
  optional(:type) => :worker | :supervisor,
  optional(:modules) => [module()] | :dynamic
}
```

### Worker Restart Options

The conditions when a worker should be restarted are dictated by its restart: option:
- :permanent - always restart
- :temporary - never restart
- :transient - restart if failed

:transient - the child process is restarted only if it terminates abnormally, i.e., with an exit reason other than :normal, :shutdown, or {:shutdown, term}.

Есть смысл запускать воркера под супервизором даже если он :temporary, и его не нужно рестартовать.
В этом случае воркер не зависнет в системе при крашах и рестартах, а гарантировано завершится.
И краш в этом воркере будет правильно логирован.


## Остановка

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


### Shutdown values (:shutdown)

The following shutdown values are supported in the :shutdown option:

:brutal_kill - the child process is unconditionally and immediately terminated using Process.exit(child, :kill).

any integer >= 0 - the amount of time in milliseconds that the supervisor will wait for its children to terminate after emitting a Process.exit(child, :shutdown) signal. If the child process is not trapping exits, the initial :shutdown signal will terminate the child process immediately. If the child process is trapping exits, it has the given amount of time to terminate. If it doesn't terminate within the specified time, the child process is unconditionally terminated by the supervisor via Process.exit(child, :kill).

:infinity - works as an integer except the supervisor will wait indefinitely for the child to terminate. If the child process is a supervisor, the recommended value is :infinity to give the supervisor and its children enough time to shut down. This option can be used with regular workers but doing so is discouraged and requires extreme care. If not used carefully, the child process will never terminate, preventing your application from terminating as well.


## Restart

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


### Supervision tree 

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
