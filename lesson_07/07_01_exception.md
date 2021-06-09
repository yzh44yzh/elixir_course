# Исключения

Как и во многих других языках, в Эликсир есть исключения. Наверняка вы уже встречались с многими из них.

Исключение **MatchError** возникает, если не сработало совпадение с образцом:
```
> {:ok, _} = {:not_ok, 42}
** (MatchError) no match of right hand side value: {:not_ok, 42}
```

Исключение **ArithmeticError** возникает при неправильных аргументах для операторов арифметики:
```
> 42 + :a
** (ArithmeticError) bad argument in arithmetic expression: 42 + :a
    :erlang.+(42, :a)
```

Вот еще некоторые:
```
> some_fun()
** (CompileError) iex:1: undefined function some_fun/0

> apply(SameModule, :some_fun, [])          
** (UndefinedFunctionError) function SameModule.some_fun/0 is undefined (module SameModule is not available)
    SameModule.some_fun()
```


## Генерация исключения с помощью raise

Функция raise генерирует исключение:
```
> raise(RuntimeError)
** (RuntimeError) runtime error
```

Можно указать аттрибут **message**, который есть у исключений всех типов:
```
> raise(RuntimeError, message: "some error")
** (RuntimeError) some error
```

RuntimeError -- этот тип исключения используется по-умолчанию, так что его можно явно не указывать:
```
> raise("some error")
** (RuntimeError) some error
```


## Перехват исключения с помощью rescue

Для перехвата исключения используется конструкция **try..rescue**. Она позволяет по-разному обрабатывать исключения разных типов:

```
iex(5)> c("07_01_exception.exs")
warning: ...
[Lesson_07.Task_01_Exception]
iex(6)> alias Lesson_07.Task_01_Exception, as: LE
Lesson_07.Task_01_Exception

iex(7)> LE.try_rescue()
This is MatchError or ArithmeticError: %MatchError{term: :b}
after clause is always called
:ok

iex(8)> r LE
iex(9)> LE.try_rescue()
This is MatchError or ArithmeticError: %ArithmeticError{message: "bad argument in arithmetic expression"}
after clause is always called
:ok

iex(10)> r LE
iex(11)> LE.try_rescue()
This is RuntimeError: %RuntimeError{message: "runtime error"}
after clause is always called
:ok

iex(12)> r LE
iex(13)> LE.try_rescue()
uknown error: %UndefinedFunctionError{arity: 0, function: :some_fun, message: nil, module: SameModule, reason: nil}
after clause is always called
:ok
```

Конструкция **after** позволяет указать код, который выполнится в любом случае, не зависимо от того, произошло исключение или нет.


## Соглашение для функций, бросающих исключения

Многие функции в Эликсир имеют два варианта. Один вариант возвращает `{:ok, Result} | {:error, Reason}`, другой вариант бросает исключение.

Например:
```
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
```
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

