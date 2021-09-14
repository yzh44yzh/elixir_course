# GenServer

Как уже было сказано, GenServer -- краеугольный камень OTP-фреймворка. Это основная абстракция в Эликсир и Эрланг проектах для взаимодействия между процессами. GenServer сам по себе используется широко, и ещё он является основой для других абстракций OTP: Supervisor, Application и GenStatem (TODO уточнить).

Хороший подход к изучению GenServer – написать его самому. Тем более, что это не так уж сложно.


## 1-й шаг, бесконечный цикл.

Нам нужен долгоживущий процесс, который не завершается без явного указания. Традиционный способ реализации такого процесса -- бесконечная рекурсия. Это возможно благодаря оптимизации хвостовой рекурсии. Мы изучали это раньше, и помним, что хвостовая рекурсия выполняется без роста стека вызовов функций, и поэтому может выполняться бесконечно. 

Реализуем процесс, который:
- запускается, 
- входит в функцию **loop**, 
- читает сообщение из почтового ящика,
- и опять входит в функцию **loop**.

Если почтовый ящик пуст, то процесс блокируется на вызове **receive** и ждет сообщений. Если прочтовый ящик не пуст, то процесс прокручивает **loop** столько раз, сколько нужно, чтобы прочитать все сообщения. На каждой итерации **loop** процесс обрабатывает одно сообщение.

```
iex(1)> c "create_gen_server/gs01.exs"
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

## 2-й шаг, завершение процесса.

Предусмотрим нормальное завершение процесса. Для этого добавим обработку сообщения `:stop`, получив которое, процесс не будет вызывать loop, и таким образом завершится. Кроме этого добавим логирование неизвестных сообщений. На данном этапе у нас все сообщения, кроме `:stop`, неизвестные.

```
iex(22)> c "create_gen_server/gs02.exs"
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


## 3-й шаг, состояние процесса.

Теперь у нас есть долгоживущий процесс. Но нужно, чтобы процесс мог хранить некое состояние -- **state**. И не только хранить, но еще и модифицировать его.

State может быть любой структурой данных. В нашем случае мы возьмем простой список. 

Мы будем передавать состояние как аргумент в функцию loop. На каждой итерации loop получает текущую версию состояния, модифицирует его, если нужно, и передает новую версию в следущий вызов loop. Таким образом состояние хранится на стеке процесса.

Процесс будет обрабатывать несколько новых сообщений:
- `{:add, item}` -- добавить новый элемент в state;
- `{:remove, item}` -- удалить элемент из state;
- `{:check, item}` -- проверить, существует ли элемент в state;
- `:show` -- показать весь state целиком.

Таким образом получилось некое АПИ для нашего сервера, которым могу пользовать другие процессы.

```
iex(43)> c "create_gen_server/gs03.exs"
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

## 4-й шаг, горячее обновление кода.

Уже на этом, довольно раннем этапе полезно иметь горячее обновление кода. Это важно, потому что мы будем постоянно модифицировать код, но при этом не хотим терять текущее, уже сформированное состояние процесса.

Но прежде давайте посмотрим, как одновременно работают две версии кода.

Мы запускаем один процесс:
```
iex(3)> alias Lesson_10.GS_4, as: S 
Lesson_10.GS_4
iex(4)> pid = S.start
start Server
Server #PID<0.119.0> enters loop
#PID<0.119.0>
iex(5)> send(pid, :show)
current state is []
:show
Server #PID<0.119.0> enters loop
```

Затем вносим в код небольшие изменения, вместо
`server_name = "Server #{inspect self()}"`
делаем
`server_name = "Server 4 #{inspect self()}"`

Перегружаем модуль и запускаем второй процесс:
```
iex(6)> r S
warning: redefining module Lesson_10.GS_4 (current version defined in memory)
  create_gen_server/gs04.exs:1

{:reloaded, Lesson_10.GS_4, [Lesson_10.GS_4]}
iex(8)> pid2 = S.start
start Server
Server 4 #PID<0.129.0> enters loop
#PID<0.129.0>
```

Дальше мы посылаем сообщения обоим серверам, и видим, что они выводят разные сообщения на "enter loop":
```
iex(9)> send(pid, :show)
current state is []
Server #PID<0.119.0> enters loop
:show
iex(10)> send(pid2, :show)
current state is []
:show
Server 4 #PID<0.129.0> enters loop

iex(12)> send(pid2, :stop)
Server 4 #PID<0.129.0> stops now
:stop
iex(13)> send(pid, :stop) 
Server #PID<0.119.0> stops now
:stop
```

То есть, в первом процессе выполняется старая версия кода, а во втором процессе новая версия.

TODO stopped here

Здесь мы заменили вызовы **loop(State)** на **?MODULE:loop(State)**.
Тем самым мы заменили локальный вызов функции (только по ее имени),
на глобальный вызов (по имени модуля и функции). Для глобального
вызова действует горячее обновление кода. Как это работает?

Нода может держать в памяти 2 версии модуля. Допустим, при создании
процесса и вызове loop, он начал выполнять версию 1 и прошел несколько
итераций рекурсии. Тем временем, мы изменили код, скомпилировали, и
загрузили в ноду версию 2. Пока текущая итерация не завершена, процесс
все еще выполняет версию 1. Но следующий вызов loop уже попадет в
версию 2.

На следующих этапах мы уже не будем останавливать процесс, а будем
пользоваться горячим обновлением кода без потери состояния сервера.

TODO: убрать лишнее, сделать понятную сессию
```
iex(14)> r S
warning: redefining module Lesson_10.GS_4 (current version defined in memory)
  create_gen_server/gs04.exs:1

{:reloaded, Lesson_10.GS_4, [Lesson_10.GS_4]}
iex(15)> pid1 = S.start
start Server 4
Server 4 #PID<0.142.0> enters loop
#PID<0.142.0>
iex(16)> send(pid1, :show)
current state is []
:show
Server 4 #PID<0.142.0> enters loop
iex(17)> r S
warning: redefining module Lesson_10.GS_4 (current version defined in memory)
  create_gen_server/gs04.exs:1

{:reloaded, Lesson_10.GS_4, [Lesson_10.GS_4]}
iex(18)> send(pid1, :show)
current state is []
:show
<Server 4> #PID<0.142.0> enters loop
iex(19)> send(pid1, {:add, 42})
<Server 4> #PID<0.142.0> enters loop
{:add, 42}
iex(20)> r S
warning: redefining module Lesson_10.GS_4 (current version defined in memory)
  create_gen_server/gs04.exs:1

{:reloaded, Lesson_10.GS_4, [Lesson_10.GS_4]}
iex(21)> send(pid1, :show)     
current state is '*'
[Server 4] #PID<0.142.0> enters loop
:show
iex(22)> send(pid1, {:add, 142})
[Server 4] #PID<0.142.0> enters loop
{:add, 142}
iex(23)> send(pid1, :show)      
current state is [142, 42]
:show
[Server 4] #PID<0.142.0> enters loop
```

## 5-й шаг, публичный АПИ модуля.

Дальше взаимодействие с сервером будет усложняться, поэтому спрячем
отправку сообщений внутри функций.

```
iex(25)> c "create_gen_server/gs05.exs"
[Lesson_10.GS_5]
iex(26)> alias Lesson_10.GS_5, as: S5
Lesson_10.GS_5
iex(27)> pid = S5.start()
start Server
[Server 5] #PID<0.171.0> enters loop
#PID<0.171.0>
iex(28)> S5.add(pid, "Hello")
[Server 5] #PID<0.171.0> enters loop
{:add, "Hello"}
iex(29)> S5.add(pid, "Bob")  
[Server 5] #PID<0.171.0> enters loop
{:add, "Bob"}
iex(30)> S5.show(pid)
current state is ["Bob", "Hello"]
:show
[Server 5] #PID<0.171.0> enters loop
iex(31)> S5.stop(pid)
[Server 5] #PID<0.171.0> stops now
:stop
```


## 6-й шаг, синхронный ответ на сообщение.

Это хорошо, что наш сервер умеет хранить состояние и менять его в
зависимости от запросов клиентов. Но было бы неплохо, чтобы сервер
умел что-нибудь отвечать клиенту.

Для этого нужно передать Pid клиента внутри сообщения.  А сервер
должен послать ответ на этот Pid.

Теперь для клиента взаимодействие с сервером выглядит как синхронный
вызовы функции, и получение ответа из нее.

TODO: нужно добавлять не цифры, а что-то другое. А то show показывает не то, что надо.
```
iex(33)> c "create_gen_server/gs06.exs"
[Lesson_10.GS_6]
iex(34)> alias Lesson_10.GS_6, as: S6
Lesson_10.GS_6
iex(35)> pid = S6.start()
start Server
[Server 6] #PID<0.195.0> enters loop
#PID<0.195.0>
iex(36)> S6.add(pid, 42)
[Server 6] #PID<0.195.0> enters loop
:ok
iex(37)> S6.add(pid, 43)
[Server 6] #PID<0.195.0> enters loop
:ok
iex(39)> S6.check(pid, 43)
[Server 6] #PID<0.195.0> enters loop
true
iex(40)> S6.check(pid, 44)
[Server 6] #PID<0.195.0> enters loop
false
iex(41)> S6.show(pid)     
[Server 6] #PID<0.195.0> enters loop
'+*'
iex(42)> S6.remove(pid, 43)
[Server 6] #PID<0.195.0> enters loop
:ok
iex(43)> S6.show(pid)      
[Server 6] #PID<0.195.0> enters loop
'*'
```

## 7-й шаг, добавляем таймаут.

Мы блокируем процесс клиента, и это не всегда хорошо. 
Будет особенно нехорошо заблокировать консоль.

Правильно будет позволить клиенту задать timeout, как долго он готов ожидать ответ сервера.

timeout можно вынести в аргументы, и предоставить каждую АПИ-функцию в
двух вариантах, с явным указанием timeout и без указания. В настоящем
gen_server так и сделано.  Но мы сейчас не будем сильно усложнять код,
а просто добавим 5-ти секундный timeout для receive.

```
iex(1)> c "create_gen_server/gs07.exs"
[Lesson_10.GS_7]
iex(2)> alias Lesson_10.GS_7, as: S7
Lesson_10.GS_7
iex(3)> pid = S7.start
start Server
[Server 6] #PID<0.117.0> enters loop
#PID<0.117.0>
iex(5)> S7.add(pid, 42)
[Server 6] #PID<0.117.0> enters loop
:ok
iex(6)> S7.show(pid)
:noreply
[Server 6] #PID<0.117.0> enters loop
```

## 8-й шаг, убираем дублирование кода в публичном АПИ.

```
iex(8)> c "create_gen_server/gs08.exs"
[Lesson_10.GS_8]
iex(9)> alias Lesson_10.GS_8, as: S8
iex(21)> pid = S8.start
start Server
[Server 6] #PID<0.149.0> enters loop
#PID<0.149.0>
iex(22)> S8.add(pid, :one)
[Server 6] #PID<0.149.0> enters loop
:ok
iex(23)> S8.add(pid, :two)
[Server 6] #PID<0.149.0> enters loop
:ok
iex(24)> S8.add(pid, :three)
[Server 6] #PID<0.149.0> enters loop
:ok
iex(25)> S8.show(pid)
[Server 6] #PID<0.149.0> enters loop
[:three, :two, :one]
iex(26)> S8.check(pid, :one)
[Server 6] #PID<0.149.0> enters loop
true
iex(27)> S8.check(pid, :four)
[Server 6] #PID<0.149.0> enters loop
false
iex(28)> S8.remove(pid, :two)
[Server 6] #PID<0.149.0> enters loop
:ok
iex(29)> S8.show(pid)
[Server 6] #PID<0.149.0> enters loop
[:three, :one]
```


## 9-й шаг, также убираем дублирование кода в loop.

После этого рефакторинга добавление новых методов АПИ сводится к добавлению публичных функций и handle_call.

Функции call и loop модифицировать теперь не нужно. И разбираться с отправкой сообщений в обе стороны тоже не нужно.

```
iex(1)> c "create_gen_server/gs09.exs"
[Lesson_10.GS_9]
iex(2)> alias Lesson_10.GS_9, as: S9
Lesson_10.GS_9
iex(3)> pid = S9.start
start Server
[Server 6] #PID<0.120.0> enters loop
#PID<0.120.0>
iex(4)> S9.add(pid, :one)
[Server 6] #PID<0.120.0> enters loop
:ok
iex(5)> S9.check(pid, :one)
[Server 6] #PID<0.120.0> enters loop
true
iex(6)> S9.stop(pid)
[Server 6] #PID<0.120.0> stops now
:stop
```


## 10-й шаг, матчинг сообщений по Ref.

Такой вариант еще далек от совершенства. Процесс-клиент любое сообщение
в своем почтовом ящике считает ответом сервера. А там могут быть совсем
другие сообщения.

Чтобы исправить эту проблему, нужно добавить уникальный идентификатор
в сообщение, и вернуть его в ответе. Для таких целей у нас есть
отдельный тип данных -- **reference**, и функция **make_ref**, которая
умеет генерировать уникальные значения такого типа.

```
iex(10)> c "create_gen_server/gs10.exs"
[Lesson_10.GS_10]
iex(11)> alias Lesson_10.GS_10, as: S10
Lesson_10.GS_10
iex(12)> pid = S10.start()
start Server
[Server 6] #PID<0.140.0> enters loop
#PID<0.140.0>
iex(13)> S10.add(pid, :two)
[Server 6] #PID<0.140.0> enters loop
:ok
iex(14)> S10.show(pid)
[Server 6] #PID<0.140.0> enters loop
[:two]
iex(15)> S10.stop(pid)
[Server 6] #PID<0.140.0> stops now
:stop
```

## 11-й шаг, монитор, обработка ошибок.

И последнее: если на сервере при обработке сообщения возникнет ошибка,
но неплохо было бы сообщить клиенту об этом.

Мы устанавливаем монитор, и теперь, если серверный процесс упадет, то
клиентский получит сообщение **{'DOWN', MRef, process, Pid, Reason}**.
Когда клиент получает ответ, он снимает монитор. Так что
монитор действует только на время обработки запроса клиента.

И нам теперь не нужно создавать ссылку с помощью make_ref, потому что
monitor возвращает аналогичную ссылку.

```
iex(1)> c "create_gen_server/gs11.exs"
warning: ...
[Lesson_10.GS_11]
iex(2)> alias Lesson_10.GS_11, as: S11
Lesson_10.GS_11
iex(3)> pid = S11.start
start Server
[Server 6] #PID<0.117.0> enters loop
#PID<0.117.0>
iex(4)> S11.check(pid, 42)
[Server 6] #PID<0.117.0> enters loop
false
iex(5)> S11.show(pid)
{:error,
 {:badarith,
  [
    {Lesson_10.GS_11, :handle_call, 2,
     [file: 'create_gen_server/gs11.exs', line: 76]},
    {Lesson_10.GS_11, :loop, 1, [file: 'create_gen_server/gs11.exs', line: 49]}
  ]}}
iex(6)> 
19:23:03.390 [error] Process #PID<0.117.0> raised an exception
** (ArithmeticError) bad argument in arithmetic expression
    create_gen_server/gs11.exs:76: Lesson_10.GS_11.handle_call/2
    create_gen_server/gs11.exs:49: Lesson_10.GS_11.loop/1
 
nil
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

- handles various kinds of edge cases
- battle-tested in production in complex concurrent systems.

The heavy lifting is done in the :gen_server module, which is included in the Erlang standard library.
Some additional wrapping is performed in the Elixir standard library, in the GenServer module.
