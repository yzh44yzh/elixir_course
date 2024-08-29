# Исключения

Как и во многих других языках, в Эликсир есть исключения. Наверняка вы уже встречались с многими из них.

Исключение **MatchError** возникает, если не сработало совпадение с образцом:

```elixir-iex
> {:ok, _} = {:not_ok, 42}
** (MatchError) no match of right hand side value: {:not_ok, 42}
```

Исключение **ArithmeticError** возникает при неправильных аргументах для операторов арифметики:

```elixir-iex
> 42 + :a
** (ArithmeticError) bad argument in arithmetic expression: 42 + :a
    :erlang.+(42, :a)
```

Вот еще некоторые:

```elixir-iex
> some_fun()
** (CompileError) iex:1: undefined function some_fun/0

> apply(SomeModule, :some_fun, [])
** (UndefinedFunctionError) function SomeModule.some_fun/0 is undefined (module SomeModule is not available)
    SomeModule.some_fun()
```

## Генерация исключения с помощью raise

Функция raise генерирует исключение:

```elixir-iex
> raise(RuntimeError)
** (RuntimeError) runtime error
```

Можно указать аттрибут **message**, который есть у исключений всех типов:

```elixir-iex
> raise(RuntimeError, message: "something happened")
** (RuntimeError) something happened
```

RuntimeError -- этот тип исключения используется по-умолчанию, так что его можно явно не указывать:

```elixir-iex
> raise("some error")
** (RuntimeError) something happened
```

## Перехват исключения с помощью rescue

Для перехвата исключения используется конструкция **try..rescue**. Она позволяет по-разному обрабатывать исключения разных типов.

Давайте проверим, как это работает:

```
defmodule ExceptionExample do
  def try_rescue() do
    try do
      :a = :b
    rescue
      error in [MatchError, ArithmeticError] ->
        IO.puts("This is MatchError or ArithmeticError: #{inspect(error)}")

      error in [RuntimeError] ->
        IO.puts("This is RuntimeError: #{inspect(error)}")

      error ->
        IO.puts("unknown error: #{inspect(error)}")
    end
  end
end
```

Внутри `rescue` можно указать несколько гардов, которые проверят тип исключения.

Код `:a = :b` вызывает `MatchError`.

```
iex(5)> c("lib/exception.exs")
[ExceptionExample]
iex(8)> alias ExceptionExample, as: E
iex(7)> E.try_rescue()
This is MatchError or ArithmeticError: %MatchError{term: :b}
```

Попробуем другое исключение:

```
  def try_rescue() do
    try do
      # :a = :b
      42 + :a
```

Здесь будет `ArithmeticError`:

```
iex(8)> r E
iex(9)> E.try_rescue()
This is MatchError or ArithmeticError: %ArithmeticError{message: "bad argument in arithmetic expression"}
```

Теперь `RuntimeError`:

```
  def try_rescue() do
    try do
      # :a = :b
      # 42 + :a
      raise(RuntimeError)

iex(10)> r E
iex(11)> E.try_rescue()
This is RuntimeError: %RuntimeError{message: "runtime error"}
```

И `UndefinedFunctionError` которое попадёт в последний гард:

```
  def try_rescue() do
    try do
      # :a = :b
      # 42 + :a
      # raise(RuntimeError)
      apply(SomeModule, :some_fun, [])

iex(12)> r E
iex(13)> E.try_rescue()
unknown error: %UndefinedFunctionError{arity: 0, function: :some_fun, message: nil, module: SameModule, reason: nil}
```

И как полагается в обработке исключений, мы можем указать блок `after`:

```
    rescue
      error in [MatchError, ArithmeticError] ->
        IO.puts("This is MatchError or ArithmeticError: #{inspect(error)}")

      error in [RuntimeError] ->
        IO.puts("This is RuntimeError: #{inspect(error)}")

      error ->
        IO.puts("unknown error: #{inspect(error)}")
    after
      IO.puts("After clause is always called")
    end
```

И он будет выполняться всегда, не зависимо от того, произошло исключение или нет:

```
iex(12)> r E
iex(13)> E.try_rescue()
unknown error: %UndefinedFunctionError{arity: 0, function: :some_fun, message: nil, module: SameModule, reason: nil}
After clause is always called
```


## Соглашение для функций, бросающих исключения

Многие функции в Эликсир имеют два варианта. Один вариант возвращает `{:ok, Result} | {:error, Reason}`, другой вариант бросает исключение.

Например:
```elixir-iex
> m = %{:a => 42}
%{a: 42}
> Map.fetch(m, :a)
{:ok, 42}
> Map.fetch(m, :b)
:error
> Map.fetch!(m, :a)
42
> Map.fetch!(m, :b)
** (KeyError) key :b not found in: %{a: 42}
    (stdlib 3.13.2) :maps.get(:b, %{a: 42})
```

Или другой пример:
```elixir-iex
> File.read("./README.md")
{:ok, "..."}
> File.read("./somefile")
{:error, :enoent}
> File.read!("./README.md")
"..."
> File.read!("./somefile")
** (File.Error) could not read file "./somefile": no such file or directory
    (elixir 1.11.3) lib/file.ex:355: File.read!/1
```

В Эликсир принято соглашение, что имя функций, бросающих исключения, должно заканчиваться восклицательным знаком.


## Использование исключений для управления потоком выполнения

Использование исключений для управления потоком выполнения (control flow) -- спорная практика. Многие рекомендуют этого избегать, другие считают это приемлемым и удобным.

Некоторые языки программирования, например Python, применяют такой подход даже в стандартных библиотеках, так что разработчик при всем желании не может этого избежать. Некоторые другие языки программирования, например Rust, вообще не имеют исключений. В Rust вместо исключений используется panic, который нельзя перехватить, и который всегда приводит к завершению процесса.

Обычная ситуация, где используются исключения для управления потоком выполения -- это длинная цепочка действий, в которой на каждом шаге может быть условие, прерывающее выполнение всей цепочки.

Например, это может быть обработка http запроса, проходящая через десятки функций принадлежащих разным модулям, и даже разным подсистемам. В этой ситуации исключения позволяют прервать обработку запроса на любом уровне и сразу вернуть ответ клиенту.

Однако, для таких ситуаций в Эликсир имеются другие средства. Поэтому применение исключений считается плохим тоном. Позже мы рассмотрим конкретные примеры.
