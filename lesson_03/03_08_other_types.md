# Другие типы

## Служебные идентификаторы

**pid** является идентификатором потока, зная который, можно отправлять потоку сообщения.

```
iex(1)> f = fn -> :ok end
#Function<45.97283095/0 in :erl_eval.expr/5>
iex(2)> pid = spawn(f)
#PID<0.110.0>
iex(3)> send(pid, :hello)
:hello
```

**port** является идентификатором специального процесса, связанного с сокетом.

```
iex(4)> :gen_tcp.listen(8080, [])
{:ok, #Port<0.6>}
iex(5)> :gen_udp.open(9090)
{:ok, #Port<0.7>}
```

**reference** является идентификатором общего назначения, который можно использовать по своему усмотрению. Например, как ключ для хранения объекта в ets таблице. Или им можно пометить сообщение, отправленное другому потоку, и ждать ответное сообщение, помеченное тем же ключом.

```
iex(6)> make_ref()
#Reference<0.2063296336.1072955394.111887>
```

BEAM гарантирует, что make_ref при каждом вызове генерирует новый уникальный ключ.


## IO List

Recursive data structure, consists of:
- byte (integer in range 0-255)
- binary
- IO List

Useful for incrementally building output that will be forwarded to an IO device (socket or file).

Не эффективно:
```
iex(8)> header = "<html><body>"
"<html><body>"
iex(9)> footer = "</body></html>"
"</body></html>"
iex(10)> name = "Bob"
"Bob"
iex(11)> body = "hello " <> name <> "!"
"hello Bob!"
iex(12)> page = header <> body <> footer
iex(15)> IO.puts page
<html><body>hello Bob!</body></html>
```

Эффективно:
```
iex(8)> header = "<html><body>"
"<html><body>"
iex(9)> footer = "</body></html>"
"</body></html>"
iex(10)> name = "Bob"
"Bob"
iex(13)> body_io = ["hello ", name, "!"]
["hello ", "Bob", "!"]
iex(14)> page_io = [header, body_io, footer]
["<html><body>", ["hello ", "Bob", "!"], "</body></html>"]
iex(16)> IO.puts page_io
<html><body>hello Bob!</body></html>
:ok
```

## function

Конечно, функциональный язык не может обойтись без анонимных функций, они же лямбды, они же замыкания. Для них тоже есть отдельный тип данных. Эти функции можно сохранять в переменную, передавать как аргументы в другие функции, и даже посылать на другую ноду, чтобы выполнить там.

```
iex(18)> f = fn(val) -> rem(val, 2) == 0 end
#Function<44.97283095/1 in :erl_eval.expr/5>
iex(19)> Enum.filter([1,2,3,4,5], f)
[2, 4]
```


## Некоторые нюансы типов данных в BEAM

Простые и составные типы данных.
Условность этого разделения (pid простой или составной?)

Сравнение значений:
number < atom < reference < function < port < pid < tuple < map < list < binary
