## Почтовый ящик

У каждого процесса есть специальная область памяти -- почтовый ящик (mailbox), куда копируются адресованные ему сообщения. Там сообщения накапливаются в очереди, в порядке их появления.

Чтобы прочитать сообщения в почтовом ящике, нужно использовать конструкцию `receive`. Она аналогична конструкции `case`, тоже состоит из шаблонов и охранных выражений.

```
iex(1)> c "09_02_mailbox.exs"
[Lesson_09.Task_02_Mailbox]
iex(2)> alias Lesson_09.Task_02_Mailbox, as: T
Lesson_09.Task_02_Mailbox

iex(3)> send(self(), {:tag1, "Hello"})
{:tag1, "Hello"}
iex(4)> T.check_mailbox()
got msg with tag1: "Hello"
:ok

iex(5)> send(self(), {:tag2, "Hi"})
{:tag2, "Hi"}
iex(6)> send(self(), :hello)
:hello
iex(7)> T.check_mailbox()
got msg with tag2: "Hi"
:ok
iex(8)> T.check_mailbox()
got unknown msg :hello
:ok
```



При вызове receive процесс берет сообщение из очереди и сопоставляет его с имеющимися шаблонами. Если находится подходящий шаблон, то выполняется соответствующий блок кода, и затем код после receive. А сообщение удаляется из почтового ящика. Другие сообщения в почтовом ящике остаются до следующего вызова receive.  Если сообщение не совпало ни с одним шаблоном, то оно остается в очереди, и для проверки берется следующее.

Receive algorithm:
- take the first message from the mailbox;
- try to match it against any of the provided patterns, going from top to bottom;
- if pattern matches the message, run the corresponding code;
- if no pattern matches, put the message back into the mailbox at the same position it originally occupied, then try the next message;
- if there are no messages in the queue, wait for a new one to arrive;
- if after clause is specified and no messages is matched in the given amount of time, run the code from after block.


Чтобы четко разобраться, как работает receive в разных ситуациях, сделаем тест.  Будем отправлять разные сообщения, и с помощью receive будем выбирать только те, которые соответствуют шаблону **{msg, Any}**.

https://github.com/yzh44yzh/practical_erlang/blob/master/08_concurrency/mb.erl

```
-module(mb).
-export([test/0]).

test() ->
    test_messages("test1, ящик пустой", []),

    test_messages("test2, одно сообщение, матчится",
                  [{msg, 1}]),

    test_messages("test3, одно сообщение, не матчится",
                  [msg1]),

    test_messages("test4, 3 сообщения, все матчатся",
                  [{msg, 1}, {msg, 2}, {msg, 3}]),

    test_messages("test5, 3 сообщения, все не матчатся",
                  [msg1, msg2, msg3]),

    test_messages("test6, 4 сообщения, часть матчится, часть не матчится",
                  [{msg, 1}, msg2, {msg, 3}, msg4]),

    test_messages("test7, 4 сообщения, часть матчится, часть не матчится",
                  [msg1, {msg, 2}, msg3, {msg, 4}]),

    ok.

test_messages(TestName, Messages) ->
    io:format("~n### ~ts~ntest_messages: ~p~n", [TestName, Messages]),
    flush(),
    [self() ! Msg || Msg <- Messages],

    io:format("call receive~n"),
    Res = receive
              {msg, M} -> {msg, M}
          after 100 -> timeout
          end,
    io:format("after receive got: ~p~n", [Res]),
    [{messages, Left}] = process_info(self(), [messages]),
    io:format("left in mailbox: ~p~n", [Left]),
    ok.

flush() ->
    receive
        _ -> flush()
    after 100 -> ok
    end.
```

Функция **flush/0** очищает почтовый ящик перед каждым тестом, для чистоты эксперимента.

По результатам теста мы видим, что receive выбирает одно сообщение, совпадающее с шаблоном, если такое есть. Если нет, то процесс блокируется.

В тесте этого не видно, но процесс блокируется либо на указанное время, либо до получения подходящего сообщения.

Почтовый ящик -- самая частая причина утечки памяти в BEAM. Если receive не вызывается, или вызывается, но обрабатывает не все сообщения, то утечка памяти неизбежна. Кроме того, по мере роста очереди, каждый проход по ней становится все медленнее и медленнее.

Эликсир и эрланг разработчики об этом знают. И если обнаруживается утечка памяти, то диагностика проблемы начинается с очередей в почтовых ящиках.

Хорошая практика -- делать в receive последний шаблон такой, чтобы он совпадал с любым сообщением. И в этом случае писать в лог о том, что процесс получил сообщение, которое он не умеет обрабатывать.

``` TODO elixir
receive
    {do_something, Data} -> do_something(Data);
    Any -> lager:warning("Got unknown message ~p", [Any])
end,
```

Это уменьшит вероятность проблем с почтовым ящиком. Но не гарантирует полностью их отсутствие. Возможно ситуация, когда сообщения поступают быстрее, чем процесс успевает их обрабатывать. Увы, но универсального рецепта для таких ситуаций нет :)


## timeout

В тесте выше уже использовался таймаут, но нужно подробнее его объяснить.

``` TODO elixir
receive
    {do_something, Data} -> do_something(Data);
end,
```

Здесь, если нет подходящего по шаблону сообщения, процесс заблокируется и будет ждать, пока сообщение появится. А если оно никогда не появится, то процесс так и останется заблокированным.

Поэтому разумно указать максимальное время, на которое можно заблокировать процесс.

``` TODO elixir
receive
    {do_something, Data} -> do_something(Data);
after
    5000 -> exp1
end,
```

Это либо целое число -- время в милисекундах, либо атом infinity. Впрочем, infinity аналогично отсутствию after.


## Регистрация процессов

Pid -- штука хорошая, но не всегда удобная. Если мы хотим посылать сообщения в процесс из нескольких других процессов, нам придется как-то передать всем им Pid получателя.

Но есть альтернатива -- процессу можно дать некое имя, глобальное на уровне всей ноды, и потом обращаться к нему по этому имени.

``` TODO elixir
register(Name, Pid)
```

Вызов register сгенерирует исключение, если имя уже связано с другим процессом.

Регистрацию процесса можно отменить:

``` TODO elixir
unregister(Name)
```

Можно узнать все имена зарегистрированных процессов, какие есть в ноде:

``` TODO elixir
registered()
```

И можно узнать Pid процесса по имени:

``` TODO elixir
whereis(Name)
```

Пробуем: 

``` TODO elixir
1> registered().
[erl_prim_loader,error_logger,kernel_safe_sup,init,user,rex,
 inet_db,kernel_sup,code_server,standard_error_sup,
 global_name_server,application_controller,file_server_2,
 user_drv,standard_error,global_group]
2> register(erl_console, self()).
true
3> registered().
[erl_prim_loader,error_logger,kernel_safe_sup,init,user,rex,
 inet_db,kernel_sup,code_server,standard_error_sup,
 global_name_server,erl_console,application_controller,
 file_server_2,user_drv,standard_error,global_group]
4> whereis(erl_console).
<0.33.0>
5> self().
<0.33.0>
6> unregister(erl_console).
true
```


## Выводы

Легкие процессы, обмен сообщениями, отсутсвие разделяемой памяти дают хорошую базу для:
- масштабируемости;
- распределенности;
- устойчивости к ошибкам.

Поскольку процессы -- дешевый ресурс, их можно создавать в количестве, адекватном нагрузке на систему, и менять вместе с изменением нагрузки.

Если мы умеем передавать сообщения от одного процессы другому в рамках одной ноды, то не сложно это делать и между двумя нодами. Нужен только транспорт поверх TCP, и в виртуальной машине  этот транспорт есть.

Поскольку процессы изолированы друг от друга, то падение одного процесса не влияет на работу других процессов.  Впрочем, устойчивость к ошибкам не появляется сама по себе, программисту еще нужно постараться, чтобы этого добиться :)
