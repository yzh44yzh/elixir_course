  - подводные камни GenServer (не пихать сюда много, или вообще перенести эту тему в middle курс)

## Отложенная инициализация

Вызов init блокирует родительский поток. А с ним и старт приложения.
А с ним и старт всей ноды. То есть, нода не начнет работу, пока все
init всех gen_server модулей не отработают.  Поэтому желательно
оставлять init легковесным и возвращаться из него как можно быстрее.

Если инициализация сервера требует долгих действий, то такие вещи
лучше делать отложено.  Например, устанавливать соединение с базой
данных, запрашивать какие-то данные из внешнего источника, создавать
большие объекты в памяти -- все это стоит делать отложено.

Есть разные способы реализовать отложенную инициализацию. Мы
рассмотрим самый простой.

Здесь в init частично инициализируется State, и
поток отправляет сообщение самому себе.

```
init(Args) ->
    State = some_light_state,
    self() ! heavy_init,
    {ok, State}.
```

Это сообщение первым ляжет в почтовый ящик, и первым будет обработано
в handle_info.

```
handle_info(heavy_init, State) ->
    NewState = heavy_state,
    {noreply, NewState};
```

После этого сервер готов обслуживать запросы клиентов.

Происходящее в handle_info не блокирует ничего, кроме потока
сервера. И поэтому вся нода в целом может стартовать быстрее, и другие
потоки быстрее начинают выполнять свою работу.


## Deadlock на gen_server:call

С этим сталкивается почти каждый новичок в Erlang, и я тоже в свое
время столкнулся.

Внутри handle\_call нельзя делать вызов gen\_server:call на самого
себя. Обычно такое получается не прямо, а опосредованно. Либо
вызывается функция из публичного АПИ, которая делает gen\_server:call,
либо вызывается какой-то другой модуль, а тот вызывает публичное АПИ
текущего модуля.

Вызов gen\_server:call, это добавление нового сообщения в почтовый
ящик. gen\_server обрабатывает сообщения по очереди. Пока он не
завершит обработку текущего сообщения, он не начнет обработку
следующего. Поэтому если обработка текущего сообщения будет ждать
результат gen\_server:call, то никогда не дождется.  1-й вызов ждет
завершения 2-го, а 2-й вызов ждет завершения 1-го. Это deadlock.

gen\_server:call по дефолту имеет таймаут в 5 секунд. Если за это
время не приходит ответ, то в потоке клиента бросается
исключение. Обычно этим и проявляется такой deadlock.  Но если вы
зачем-то заменили дефолтный таймаут на infinity, то поток в таком
состоянии будет висеть бесконечно.  В какой-то момент его очередь
сообщений исчерпает всю доступную оперативную память, и нода упадет.

Выводы:
- помните про такой deadlock;
- хорошо продумывайте таймауты.



## GenServer problem

### Mailbox

Theoretically, a process mailbox has an unlimited size. In practice, the mailbox size is limited by available memory.
If messages arrive faster than the process can handle them, the mailbox will constantly grow and increasingly consume memory. Ultimately, a single process may cause an entire system to crash by consuming all the available memory. 
Solution: Optimize message handling and/or split server into multiple processes.

In addition, large mailbox cause performance slowdowns. 
When a new message arrives, receive iterates through all existed messages before processing the new one.
Solution: catch-all receive clause that deal with unexpected kinds of messages.

Deep-copying is an in-memory operation, so it should be reasonably fast. 
Having many processes frequently send big messages may affect system performance.

A special case where deep-copying doesn't take place involves binaries larger than 64 bytes. 
(TODO kbytes?)
These are maintained on a special shared binary heap. 

### Long init

delayed init, different implementations


### call timeout

When request times out, it isn't removed from the mailbox. 
A timeout means you give up waiting on the response, but the message is still in mailbox and will be processed at some point.


### Deadlock 
