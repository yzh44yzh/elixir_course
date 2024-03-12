# Сложные типы (Complex types)

## IO List

Рекурсивный тип данных, который состоит из:
- [byte] -- список байт, числа в диапазоне 0-255
- String.t -- бинарная строка
- IO List -- включает сам себя

```
iex(1)> [0, 127, 255] |> IO.iodata_to_binary
<<0, 127, 255>>
iex(2)> [0, 127, 255, 256] |> IO.iodata_to_binary
** (ArgumentError) errors were found at the given arguments:
iex(3)> [97, 98, 99] |> IO.iodata_to_binary
"abc"
iex(4)> [97, 98, 99, [100, 101]] |> IO.iodata_to_binary
"abcde"
iex(5)> [97, 98, 99, [100, 101], " Hello"] |> IO.iodata_to_binary
"abcde Hello"
iex(6)> [97, 98, 99, [100, 101], " Hello", [" ", "world", 33]] |> IO.iodata_to_binary
"abcde Hello world!"
```

Такой тип поддерживается во всех операциях ввода-вывода и позволяет избежать дорогостоящей конкатенации строк.

Не эффективно:

```elixir-iex
iex(1)> header = "<html><body>"
"<html><body>"
iex(2)> footer = "</body></html>"
"</body></html>"
iex(3)> name = "Bob"
"Bob"
iex(4)> title = "Hello " <> name <> "!"
"Hello Bob!"
iex(5)> page = "<h1>" <> title <> "</h1>"
"<h1>Hello Bob!</h1>"
iex(6)> html = header <> page <> footer
"<html><body><h1>Hello Bob!</h1></body></html>"
```

Здесь много операций по копированию строк из одной области памяти в другую, чтобы в итоге результат разместился в одной непрерывной области памяти.

Эффективно:

```elixir-iex
iex(7)> title = ["Hello", name, "!"]
["Hello", "Bob", "!"]
iex(8)> page = ["<h1>", title, "</h1>"]
["<h1>", ["Hello", "Bob", "!"], "</h1>"]
iex(9)> html = [header, page, footer]
["<html><body>", ["<h1>", ["Hello", "Bob", "!"], "</h1>"], "</body></html>"]
iex(10)> IO.puts(html)
<html><body><h1>HelloBob!</h1></body></html>
```

Здесь нет копирования, а результат представляет собой дерево из ссылок на разные области памяти.

Применяется везде, где есть IO: запись в файл, запись в сокет.


## Keyword List

Когда-то давно BEAM не поддерживала словари, и основной структурой данных "ключ-значение" был Keyword List.

Это список кортежей из двух элементов, где первых элемент является ключом, а второй элемент -- значением. Ключ должен быть атомом.

```
iex(6)> my_dict = [{:a, 42}, {:b, 100}, {:c, 500}]
[a: 42, b: 100, c: 500]
iex(7)> my_dict[:a]
42
iex(8)> my_dict[:b]
100
iex(9)> my_dict[:d]
nil
```

И хотя это просто список кортежей, здесь работает синтаксический сахар, похожий на словарь:

```
iex(11)> my_dict = [a: 42, b: 100]
[a: 42, b: 100]
```

Есть модуль **Keyword**, который предоставляет АПИ, похожее на АПИ словарей:

```
iex(12)> Keyword.fetch(my_dict, :a)
{:ok, 42}
iex(13)> Keyword.fetch(my_dict, :d)
:error
```

И со списком можно работать как с любым другим списком -- добавлять новые значения в начало:

```
iex(15)> my_dict = [{:c, 100} | my_dict]
[c: 100, a: 42, b: 100]
```

После появления словарей этот тип данных вроде бы и не нужен. Но по сложившейся традиции активно используется в трёх случаях:

- передача необязательных параметров (настроек) в функции,
- конфигурационные файлы,
- DSL языки, основаные на макросах.

Первый случай мы уже видели на примере функции `String.split/3`:

```
iex(16)> h String.split
def split(string, pattern, options \\ [])
```

Здесь 3-й аргумент `options`, это keyword list.

Внешне это выглядит, как некие именованые аргументы:

```
iex(18)> String.split("a b c", " ", parts: 2)
["a", "b c"]
iex(19)> String.split("a b  c", " ")
["a", "b", "", "c"]
iex(20)> String.split("a b  c", " ", trim: true)
["a", "b", "c"]
iex(21)> String.split("a b  c", " ", parts: 5, trim: true)
["a", "b", "c"]
```

Но на самом деле это синтаксический сахар:

```
iex(22)> String.split("a b  c", " ", [parts: 5, trim: true])
["a", "b", "c"]
iex(23)> String.split("a b  c", " ", [{:parts, 5}, {:trim, true}])
["a", "b", "c"]
```

В стандартной библиотеке и в сторонних библиотеках не редко встречаются такие функции, которые принимают агрумент `options` как keyword list:

```
iex(26)> String.replace("Hello world", "o", "A")
"HellA wArld"
iex(27)> String.replace("Hello world", "o", "A", global: false)
"HellA world"
```

А вот пример конфигурационного файла с параметрами подключения к базе данных:

```
config :my_service, MyService.Repo,
  username: "some_user",
  password: "secret-pass",
  database: "my_service_db",
  hostname: "db.host",
  pool_size: 10
```

Это тоже keyword list, причём вложенный. Без синтаксического сахара он будет выглядеть так:

```
config(:my_service, [
  {
    MyService.Repo, [
      {:username, "some_user"},
      {:password, "secret-pass"},
      {:database, "my_service_db"},
      {:hostname, "db.host"},
      {:pool_size, 10}
    ]
  }
])
```

И третий случай рассмотрим на примере библиотеки **Ecto** для работы с базами данных. Часть этой библиотеки **Ecto.Query** представляет собой DSL для составления запросов, и он полностью построен на макросах и keyword list.

Типичный запрос выглядит так:

```
from u in "users",
  where: u.age > 18,
  select: u.name
```

А без синтаксического сахара вот так:

```
from(u in "users", [
  {:where, u.age > 18},
  {:select, u.name}
])
```

Здесь `from` это не функция, а макрос. А его второй аргумент -- keyword list.


