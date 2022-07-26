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

## Keyword List

Легаси со времен, когда EVM не поддерживала map. 

Активно используются до сих пор, хотя вроде бы незачем.

Сложившаяся традиция:
options -- настройки

TODO примеры

в отличие от map сохраняют порядок ключей


## Range

TODO
