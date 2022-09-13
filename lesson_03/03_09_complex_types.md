# Сложные типы

## IO List

Recursive data structure, consists of:
- byte (integer in range 0-255)
- String.t (binary)
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

Здесь много операций по копированию строк из одной области памяти в другую, чтобы в итоге результат разместился в одной непрерывной области памяти.

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

Здесь нет копирования, а результат представляет собой дерево из ссылок на разные области памяти.


## Keyword List

Легаси со времен, когда BEAM не поддерживала map. 

Список кортежей из двух элементов, где первых элемент -- atom() -- ключ, а второй элемент -- значение.

Активно используются до сих пор, хотя вроде бы незачем.

Сложившаяся традиция:
options -- настройки
Много функций в стандартной библиотеке принимают список options в виде keyword list.
TODO: например.

TODO примеры

[{:a, 42}, {:b, 50}]
[a: 42, b: 50]

в отличие от map сохраняют порядок ключей


## Range

TODO

1..10
a in range
iterate over range

Build on top of map.

## Sigil

TODO

## Date, Time, DateTime

TODO
