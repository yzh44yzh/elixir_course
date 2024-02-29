# Почтовый ящик

У каждого процесса есть специальная область памяти -- почтовый ящик (mailbox), куда копируются адресованные ему сообщения. Там сообщения накапливаются в очереди в порядке их появления.

Чтобы прочитать сообщения в почтовом ящике, нужно использовать конструкцию `receive`. Она аналогична конструкции `case`, тоже состоит из шаблонов и охранных выражений.

```elixir-iex
iex(1)> c "lib/mailbox.exs"
[MailboxExample]
iex(2)> alias MailboxExample, as: M
iex(3)> send(self(), {:tag1, "Hello"})
{:tag1, "Hello"}
iex(4)> M.check_mailbox()
got msg with tag1: "Hello"
:ok

iex(5)> send(self(), {:tag2, "Hi"})
{:tag2, "Hi"}
iex(6)> send(self(), :hello)
:hello
iex(7)> M.check_mailbox()
got msg with tag2: "Hi"
:ok
iex(8)> M.check_mailbox()
got unknown msg :hello
:ok
```

## receive .. after

Если в момент вызова receive почтовый ящик пуст, то процесс блокируется и ждет сообщений. Часто нужно ограничить время ожидания. Для этого используется `receive .. after`.

```elixir-iex
iex(3)> M.check_mailbox(1000)
no messages after 1000 ms
:ok
iex(4)> send(self(), :hello)
:hello
iex(5)> M.check_mailbox(1000)
got msg :hello
:ok
```

## receive действует избирательно

receive забирает из почтового ящика только одно сообщение, первое, подходящее по шаблону. Остальные сообщения остаются в очереди и ждут.

```elixir-iex
iex(8)> send(self(), 41)
41
iex(9)> send(self(), 42)
42
iex(10)> send(self(), 43)
43
iex(11)> M.check_for_42()
got 42
:ok
iex(12)> flush()
41
43
:ok
```

Если подходящего сообщения нет, то все сообщения остаются в очереди.

```elixir-iex
iex(13)> send(self(), 31)
31
iex(14)> send(self(), 32)
32
iex(15)> send(self(), 33)
33
iex(16)> M.check_for_42()
:ok
iex(17)> flush()
31
32
33
```


## Переполнение почтового ящика

Размер почтового ящика не ограничен. Если процесс забирает не все сообщения или забирает их медленнее, чем они поступают, то очередь растет.

Чем больше очередь, тем медленнее работает receive, потому что ему нужно проверить все сообщения в очереди по своим шаблонам. 

Это одна из частых проблем в BEAM. Из-за растущих очередей система сперва замедляется, затем потребляет всю доступную память и аварийно завершается. Поэтому разработчики всегда следят за тем, чтобы процессы успевали потреблять все входящие сообщения. 

Хорошая практика -- делать в receive последний шаблон такой, чтобы он совпадал с любым сообщением. И в этом случае писать в лог предупреждение о том, что процесс получил сообщение, которое он не умеет обрабатывать. Это называется *catch all* шаблон.

```elixir-iex
iex(19)> send(self(), :something_not_expected)
:something_not_expected
iex(20)> T.check_mailbox()
got unknown msg :something_not_expected
:ok
```

Это не гарантирует защиту от переполнения, но уменьшает вероятность проблем. 

Это является хорошей практикой еще и потому, что если процесс получает неожиданные сообщения, то скорее всего где-то в коде есть баг -- сообщения отправляются не туда, куда должны отправляться. catch all шаблон позволяет заметить это. 


## Регистрация процессов

Pid не единственный способ обратиться к нужному процессу. Если мы хотим отправлять сообщения из нескольких других процессов, что бывает нередко, то довольно сложно всем им передать нужный Pid. А если учесть, что многие процессы живут не бесконечно, а стартуют и завершаются, то пользоваться Pid становится совсем неудобно. 

Процесс можно зарегистрировать под неким именем и затем обращаться к нему по этому имени. 

```elixir-iex
iex(3)> Process.register(self(), :pool_manager)
true
iex(4)> send(:pool_manager, :hello)
:hello
iex(5)> M.check_mailbox()
got unknown msg :hello
:ok
```

Имена глобальны на уровне всей системы, и в любой момент времени под конкретным именем может быть зарегистрирован только один процесс.

```elixir-iex
iex(6)> Process.register(self(), :pool_manager)
** (ArgumentError) could not register #PID<0.107.0> with name :pool_manager because it is not alive, the name is already taken, or it has already been given another name
```

Как видно из этого сообщения, зарегистрировать процесс не удается, если:
- имя уже занято другим процессом;
- нужный процесс уже зарегистрирован с другим именем;
- нужный процесс уже завершился (not alive).

Мы можем узнать все имена, которые зарегистрированы в системе:

```elixir-iex
iex(7)> Process.registered()
[:global_group, :elixir_config, :file_server_2, :pool_manager, :code_server,
 :kernel_sup, IEx.Pry, Logger, :erl_prim_loader, :elixir_code_server,
 IEx.Config, :standard_error, :erts_code_purger, :application_controller,
 IEx.Broker, :user_drv, Logger.BackendSupervisor, :logger_sup, :kernel_safe_sup,
 :kernel_refc, :init, :user, :logger_proxy, :logger, :standard_error_sup,
 :global_name_server, IEx.Supervisor, :elixir_sup, :erl_signal_server,
 Logger.Supervisor, :inet_db, :socket_registry, :logger_handler_watcher, :rex]
```

Как видим, их довольно много.

Мы можем узнать Pid процесса по имени:

```elixir-iex
iex(11)> Process.whereis(:pool_manager)
#PID<0.107.0>
```

Регистрацию процесса можно отменить:

```elixir-iex
iex(13)> Process.unregister(:pool_manager)
true
iex(14)> Process.whereis(:pool_manager)
nil
```
