# Строки и бинарные данные (String & Binary)

В эликсир есть два вида строк. Строки в одинарных кавычках представляют собой последовательность Unicode-символов, где каждый символ представлен 32-х разрядным числом. То есть, он занимает 4 байта. Такие строки используются не часто.

Гораздо чаще используются строки в двойных кавычках, которые представляют собой бинарные данные в формате UTF-8. В этом формате символы латинского алфавита занимают 1 байт, символы кириллицы занимают 2 байта, а символы других алфавитов занимают от 1 до 4 байт. То есть, такая строка занимает значительно меньше памяти.

```elixir-iex
iex(1)> i 'Hello'
Data type
  List
Raw representation
  [72, 101, 108, 108, 111]

iex(2)> i "Hello"
Data type
  BitString
Raw representation
  <<72, 101, 108, 108, 111>>

iex(3)> i 'Привет'
Term
  [1055, 1088, 1080, 1074, 1077, 1090]
Data type
  List

iex(4)> i "Привет"
Data type
  BitString
Raw representation
  <<208, 159, 209, 128, 208, 184, 208, 178, 208, 181, 209, 130>>
```

Для склеивания бинарных строк применяется оператор `<>`:

```elixir-iex
iex(5)> "Hello" <> ", " <> "World!"  # "Hello, World!"
"Hello, World!"
```

А для склеивания списков `++`:

```elixir-iex
iex(6)> 'Hello, ' <> 'World!'
** (ArgumentError) expected binary argument in <> operator but got: ~c"Hello, "
iex(7)> 'Hello, ' ++ 'World!'
~c"Hello, World!"
```

Модуль `String` из стандартной библиотеки содержит функции для работы со строками. Например, функцию для определения длины строки `String.length(my_str)`:

```elixir-iex
iex(1)> String.length("Hello world!")
12
iex(2)> String.length("Привет мир!")
11
```

Функцию для разбиения строки на части `String.split(my_str, separator)`:

```elixir-iex
iex(8)> String.split("aa bb cc")
["aa", "bb", "cc"]
iex(9)> String.split("aa-bb-cc", "-")
["aa", "bb", "cc"]
iex(10)> String.split("aa-bb_cc=dd", ["-", "_", "="])
["aa", "bb", "cc", "dd"]
```

Функцию для удаления пробельных символов `String.trim(my_str)`:

```elixir-iex
iex(5)> String.trim(" Hello world!  ")
"Hello world!"
iex(6)> String.trim(" Hello world!\n  ")
"Hello world!"
iex(7)> String.trim("\t Hello\tworld!\n  ")
"Hello\tworld!"
```

И еще несколько десятков [функций](https://hexdocs.pm/elixir/1.12/String.html#functions).

Одна из операций над строками, которая на первый взгляд кажется простой, это перевод строки в верхний или нижний регистр. На самом деле для некоторых алфавитов эта операция должна учитывать контекст, а не просто один символ. А для некоторых других алфавитов она не имеет смысла.

Функция `String.upcase(str, mode)` работает в трех разных режимах. В режиме `:default` она переводит в верхний регистр все символы, для которых это возможно. В режиме `:ascii` она переводит только символы латинского алфавита:

```elixir-iex
iex(10)> String.upcase("hello, мир!", :default)
"HELLO, МИР!"
iex(11)> String.upcase("hello мир!", :ascii)
"HELLO, мир!"
```

Третий режим -- `:greek` -- используется для греческого алфавита, где как раз эта операция зависит от контекста.

два вида строк


## Упражнение №1: "align words"

В файле lib/string_example.exs рассматривается задача выравнивания слов по центру.

```elixir-iex
iex(1)> c "lib/string_example.exs"
[StringExample, StringExampleTest]
iex(2)> words = ~w'cat zebra elephant'
["cat", "zebra", "elephant"]
iex(3)> Enum.map(words, &String.length/1)
[3, 5, 8]
```

Запускаем тесты:

```shell
elixir lib/string_example.exs
...
Finished in 0.05 seconds (0.05s on load, 0.00s async, 0.00s sync)
3 tests, 0 failures

Randomized with seed 312660
```

## Упражнение №2: парсинг get-параметров

TODO

foo=bar&token=value&bar=baz

corner cases:
foo=bar&token&bar=baz
foo=bar&token=some=value&bar=baz
token=some=value
