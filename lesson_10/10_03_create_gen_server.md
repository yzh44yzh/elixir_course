Хороший подход к изучению gen_server – написать его самому. Такой подход выбрали и Joe Armstrong (Programming Erlang, глава 16), и Fred Hebert (LYSE, глава What is OTP?). И Sasa Juric.

Пойдем и мы тем же путем, напишем свой gen_server.

## Building a generic server process

To implement a server process you need to do the following:
- spawn a separate process
- run an infinite loop in the process
- maintain the process state
- react to messages
- send a response back to the caller

Sasa Jiric делает свою реализацию GenServer сразу с разделением на generic code and concrete implementation.

Elixir ships with a much better support for generic server process, called GenServer:
- much more feature rich
- handles various kinds of edge cases
- battle-tested in production in complex concurrent systems.

The heavy lifting is done in the :gen_server module, which is included in the Erlang standard library.
Some additional wrapping is performed in the Elixir standard library, in the GenServer module.

GenServer.start works synchronously. It returns only after init/1 callback has finished in server process.
Client process is blocked until the server process is initialized.

GenServer.call doesn't wait indefinitely for a responce. 5 sec timeout by default.

If server process terminates while client is waiting for resonce, GenServer detects it and raises a corresponding error in the client process. 

If init/1 returns {:stop, reason} client will receive {:error, reason}.
If init/1 returns :ignore, client will receive :ignore.
The first is an error situation, the second is a normal situation.


## 1-й этап, простейший цикл.

Нам нужен поток, который никогда не завершается. Используем для этого
бесконечную рекурсию.

Поток запускается, входит в функцию **loop**, проверяет почтовый ящик,
обрабатывает сообщения, и опять входит в функцию **loop**.  Тут важно,
чтобы это была хвостовая рекурсия. Иначе будет расти память на стеке и
в какой-то момент нода упадет из-за нехватки памяти.

```
iex(1)> c "create_gen_server/gs1.exs"
[Lesson_10.GS_1]
iex(2)> alias Lesson_10.GS_1, as: S
Lesson_10.GS_1

iex(13)> pid = S.start()
start Server
Server #PID<0.140.0> enters loop
#PID<0.140.0>
iex(14)> send(pid, "Hello")
Server #PID<0.140.0> got msg "Hello"
"Hello"
Server #PID<0.140.0> enters loop
iex(15)> send(pid, 42)
42
Server #PID<0.140.0> got msg 42
Server #PID<0.140.0> enters loop

```

## 2-й этап, stop

Предусмотрим нормальное завершение потока. Для этого добавим обработку
сообщения **stop**, получив которое, поток не будет вызывать **loop**.
И, таким образом, завершится.

Логируем получение неизвестных сообщений.

```
iex(22)> c "create_gen_server/gs2.exs"
[Lesson_10.GS_2]
iex(23)> alias Lesson_10.GS_2, as: S2
Lesson_10.GS_2
iex(24)> pid = S2.start()
start Server
Server #PID<0.168.0> enters loop
#PID<0.168.0>
iex(25)> send(pid, "Hello")
ERROR: Server #PID<0.168.0> got unknown msg "Hello"
Server #PID<0.168.0> enters loop
"Hello"
iex(26)> send(pid, 42)     
ERROR: Server #PID<0.168.0> got unknown msg 42
42
Server #PID<0.168.0> enters loop
iex(27)> send(pid, :stop)
Server #PID<0.168.0> stops now
:stop
iex(28)> Pro
Process     Protocol    
iex(28)> Process.alive?(pid)
false
```


## 3-й этап, цикл c состоянием.

Добавим хранимое состояние. Теперь функция **loop** получает
аргумент. Это состояние потока. После **spawn** он имеет некое
начальное состояние. В данном случае это массив. Но это может быть
любая структура данных.

Затем поток может модифицировать эту структуру, и в последующие вызовы
**loop** передавать новое состояние. Таким образом, не имея
изменяемых переменных, мы все-таки имеем изменяемое состояние потока в
его стеке.

Обрабатываем сообщения: add, remove, check, show.  Тут мы усложнили
форматы сообщений, которые умеет обрабатывать поток.  И сформировали
некое АПИ: добавление и удаление элементов и вывод их на консоль.

```
iex(43)> c "create_gen_server/gs3.exs"
[Lesson_10.GS_3]
iex(44)> alias Lesson_10.GS_3, as: S3
Lesson_10.GS_3
iex(45)> pid = S3.start()
start Server
Server #PID<0.202.0> enters loop
#PID<0.202.0>
iex(46)> send(pid, {:add, 1})
Server #PID<0.202.0> enters loop
{:add, 1}
iex(47)> send(pid, {:add, 42})
Server #PID<0.202.0> enters loop
{:add, 42}
iex(48)> send(pid, {:check, 1})
check true
{:check, 1}
Server #PID<0.202.0> enters loop
iex(49)> send(pid, {:check, 42})
check true
{:check, 42}
Server #PID<0.202.0> enters loop
iex(50)> send(pid, :show)       
current state is [42, 1]
:show
Server #PID<0.202.0> enters loop
iex(51)> send(pid, {:delete, 42})
ERROR: Server #PID<0.202.0> got unknown msg {:delete, 42}
{:delete, 42}
Server #PID<0.202.0> enters loop
iex(52)> send(pid, {:remove, 42})
Server #PID<0.202.0> enters loop
{:remove, 42}
iex(53)> send(pid, {:check, 42}) 
check false
{:check, 42}
Server #PID<0.202.0> enters loop
iex(54)> send(pid, :show)       
current state is [1]
:show
Server #PID<0.202.0> enters loop
iex(55)> send(pid, :stop)
Server #PID<0.202.0> stops now
:stop
```

## N-й этап, горячее обновление кода.

src/gs3.erl

Здесь мы заменили вызовы **loop(State)** на **?MODULE:loop(State)**.
Тем самым мы заменили локальный вызов функции (только по ее имени),
на глобальный вызов (по имени модуля и функции). Для глобального
вызова действует горячее обновление кода. Как это работает?

Нода может держать в памяти 2 версии модуля. Допустим, при создании
потока и вызове loop, он начал выполнять версию 1 и прошел несколько
итераций рекурсии. Тем временем, мы изменили код, скомпилировали, и
загрузили в ноду версию 2. Пока текущая итерация не завершена, поток
все еще выполняет версию 1. Но следующий вызов loop уже попадет в
версию 2.

На следующих этапах мы уже не будем останавливать поток, а будем
пользоваться горячим обновлением кода.


## N-й этап, публичный АПИ модуля.

src/gs4.erl

Дальше взаимодействие с сервером будет усложняться, поэтому спрячем
отправку сообщений внутри функций.


## N-й этап, синхронный ответ на сообщение.

src/gs5.erl

Это хорошо, что наш сервер умеет хранить состояние и менять его в
зависимости от запросов клиентов. Но было бы неплохо, чтобы сервер
умел что-нибудь отвечать клиенту.

Для этого нужно передать Pid клиента внутри сообщения.  А сервер
должен послать ответ на этот Pid.

Теперь для клиента взаимодействие с сервером выглядит как синхронный
вызовы функции, и получение ответа из нее.


## N-й этап, добавляем таймаут.

src/gs6.erl

Мы блокируем поток клиента, и это не всегда хорошо. Правильно будет позволить
клиенту задать timeout, как долго он готов ожидать ответ сервера.

timeout можно вынести в аргументы, и предоставить каждую АПИ-функцию в
двух вариантах, с явным указанием timeout и без указания. В настоящем
gen_server так и сделано.  Но мы сейчас не будем сильно усложнять код,
а просто добавим 5-ти секундный timeout в функцию receive.


## N-й этап, убираем дублирование кода в публичном АПИ.

src/gs7.erl


## N-й этап, также убираем дублирование кода в loop.

src/gs8.erl


## N-й этап, матчинг сообщений по Ref.

src/gs9.erl

Такой вариант еще далек от совершенства. Поток-клиент любое сообщение
в своем почтовом ящике считает ответом сервера. А там могут быть совсем
другие сообщения.

Чтобы исправить эту проблему, нужно добавить уникальный идентификатор
в сообщение, и вернуть его в ответе. Для таких целей у нас есть
отдельный тип данных -- **reference**, и функция **make_ref**, которая
умеет генерировать уникальные значения такого типа.


## N-й этап, монитор, обработка ошибок.

src/gs10.erl

И последнее: если на сервере при обработке сообщения возникнет ошибка,
но неплохо было бы сообщить клиенту об этом.

Тут мы немного забегаем вперед, ибо обработка ошибок в одном потоке из
другого потока, это тема следующих уроков. Но хорошая реализация
требует, так что делаем :)

Мы устанавливаем монитор, и теперь, если серверный поток упадет, то
клиентский получит сообщение **{'DOWN', MRef, process, Pid, Reason}**.
Когда клиент получает ответ, он снимает монитор. Так что
монитор действует только на время обработки запроса клиента.

И нам теперь не нужно создавать ссылку с помощью make_ref, потому что
monitor возвращает аналогичную ссылку.

```
gs10:call(Pid, hello)
```

## Итог

Итак, мы написали свой gen_server. Мы умеем:
- принимать запросы от клиента и отвечать на них;
- хранить и модифицировать состояние;
- делать горячее обновление кода;
- обрабатывать ошибки.

Настоящий gen\_server, входящий в состав OTP, устроен сложнее, конечно.
Но концептуально он работает именно так.

Код gen_server отшлифован за многие годы использования во многих
высоконагруженных проектах.  Так что нужно пользоваться именно им. И в
следующем уроке мы научимся этому.
