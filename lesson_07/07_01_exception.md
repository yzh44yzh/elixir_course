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


## raise -- генерация исключения

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


## rescue -- перехват исключения

**rescue**, **catch**, and **after** clauses are optional.

The rescue and catch clauses look a bit like the body of a **case**.
They take patterns and code to execute if the pattern matches.
The subject of the pattern is the exception that was raised.

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

We define three different exception patterns.
The first matches one of the two exceptions, FunctionClauseError or RuntimeError.
The second matches an ArithmeticError and stores the exception value in the variable error.
And the last clause catches any exception into the variable other_error.

We also include an after clause.
This will always run at the end of the try function, regardless of whether an exception was raised.



## Соглашение для функций, бросающих исключения

Most of the time you can easily identify the functions that can raise errors or
throw values because their names end with an exclamation point. For example,
the File.cd!/1 function raises an exception when the path doesn’t exist.

File.open!
The trailing exclamation point in the method name is an Elixir convention —
if you see it, you know the function will raise an exception on error, and that exception will be meaningful.

Many built-in functions have two forms.
The **xxx** form returns the tuple {:ok, data}
and the **xxx!** form returns data on success but raises an exception otherwise.


## Использование исключений для control flow

```
def checkout() do
  try do
    {quantity, _} = ask_number("Quantity?")
    {price, _} = ask_number("Price?")
    quantity * price
  rescue
    MatchError -> "It's not a number"
  end
end
```

Inside the try block, we create the happy-path code. The happy path is the
code that handles only the success scenario. Then, in the rescue block, we
create the error-handling code. Still in the rescue block, for each line we should
provide an exception struct to match, and a code block.

When the pattern
matching fails the MatchError exception will be raised, and then the list of pat-
tern-matching expressions in the rescue will try to match the exception and
execute the code block.

If none of the pattern-matching expressions matches
an exception raised, Elixir will raise that exception again.

В некоторых языках исключения используются как control flow. Например, в Python нельзя этого избежать, даже если не хочется. Но в функциональных языках использование исключений для control flow считается плохим тоном.

Позже мы рассмотрим, какие есть альтернативы такому подходу.

First, the official warning: exceptions in Elixir are not control-flow structures.
Instead, Elixir exceptions are intended for things that should never happen in normal operation.

Throwing values or raising errors is unusual in functional programming. However,
in large applications you’ll install libraries from other developers that use this
strategy, and you need to know how to properly handle the raised errors and
thrown values.


