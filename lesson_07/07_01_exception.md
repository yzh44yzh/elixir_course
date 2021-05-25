# Обработка ошибок


## Defensive Programming vs Let It Crash

Когда вся программа выполняется в одном потоке, аварийное завершение
этого потока означает аварийное завершение программы. И если это
случилось в месте, где явно не предусмотрена обработка ошибок, то
остается минимум информации для диагностики проблемы.

Поэтому программисты стараются предусмотреть обработку всех возможных
ошибок во всех возможных местах. Такой стиль программирования
называется **Defensive Programming**. И он нередко приводит к тому,
что программа содержит больше кода для обработки ошибок, чем кода,
выполняющего основную задачу. Конечно, это усложняет и написание кода,
и поддержку.

Эликсир предлагает другой подход: реализовать только основную задачу
(**happy path**) и не писать код для обработки ошибок. Благодаря
многопоточности и разделению потоков на рабочие и супервизоры, любая
ошибка всегда будет замечена и записана в лог. А система в целом
продолжит работу. Этот подход называется **Let It Crash**.

Между тем, все инструменты для Defensive Programming в эрланг есть.
И полностью от этого подхода никто не отказывается.  На практике
каждый разработчик ищет свой баланс между Defensive Programming
и Let It Crash.

Almost all languages, including Elixir, have built-in mechanisms to handle
exceptions. These require that we identify risky code in advance, wrap it in
a block that tries to execute it, and provide a block to rescue the situation if
the code fails. In most languages, this kind of exception handling is essential,
but in Elixir we hardly ever have to reach for it.

it’s nearly impossible to predict all possible failures
in advance, so they decided to focus on recovering from failure instead.


# Exceptions

Показать примеры:
- failed pattern matching
- timeout on GenServer call
- invalid arithmetic (division by zero, 1 + :a)
- invocation of non-existing function

Краш одного потока не влияет на остальные потоки (кроме супервизора)
Это отличается от некоторых других ЯП, где несколько потоков реализованы в рамках одного системного процесса, и краш одного потока крашит весь процесс и все потоки в нем
(TODO узнать подробнее об этом)


Elixir (like Erlang) takes the view that errors should normally be fatal
to the processes in which they occur.

You won’t find much exception-handling code in Elixir programs.
Exceptions are raised, but you rarely catch them.

The Elixir source code for the mix utility contains no exception handlers.
The Elixir compiler itself contains a total of five
(but it is doing some pretty funky things).

If you find yourself defining new exceptions,
ask if you should be isolating the code in a separate process instead.
After all, if it can go wrong, wouldn’t you want to isolate it?

В некоторых языках исключения используются как control flow. Например, в Python нельзя этого избежать, даже если не хочется. Но в функциональных языках использование исключений для control flow считается плохим тоном.


First, the official warning: exceptions in Elixir are not control-flow structures.
Instead, Elixir exceptions are intended for things that should never happen in normal operation.

You use exceptions far less in Elixir than in other languages —
the design philosophy is that errors should propagate back up to an external, supervising process.

Throwing values or raising errors is unusual in functional programming. However,
in large applications you’ll install libraries from other developers that use this
strategy, and you need to know how to properly handle the raised errors and
thrown values.

Raise an exception with the raise function.
At its simplest, you pass it a string and it generates an exception of type RuntimeError.
```
iex> raise "Giving up"
** (RuntimeError) Giving up
```

You can also pass the type of the exception, along with other optional attributes.
All exceptions implement at least the message attribute.
```
iex> raise RuntimeError
** (RuntimeError) runtime error
iex> raise RuntimeError, message: "override message"
** (RuntimeError) override message
```

Most of the time you can easily identify the functions that can raise errors or
throw values because their names end with an exclamation point. For example,
the File.cd!/1 function raises an exception when the path doesn’t exist.

File.open!
The trailing exclamation point in the method name is an Elixir convention —
if you see it, you know the function will raise an exception on error, and that exception will be meaningful.

Many built-in functions have two forms.
The **xxx** form returns the tuple {:ok, data}
and the **xxx!** form returns data on success but raises an exception otherwise.

When runtime error happens, execution control is transfered up the call stack to the error-handling code.
If you didn't specify such code, the process, where the error happened, is terminated.
All other processes run unaffected.

3 types of runtime errors:
- throw
- error
- exit


## Raise

```
iex> raise "Giving up"
** (RuntimeError) Giving up

iex> raise RuntimeError
** (RuntimeError) runtime error

iex> raise RuntimeError, message: "override message"
** (RuntimeError) override message
```

You can also pass the type of the exception, along with other optional fields.
All exceptions implement at least the message field.


## try..rescue

**rescue**, **catch**, and **after** clauses are optional.

The rescue and catch clauses look a bit like the body of a **case**.
They take patterns and code to execute if the pattern matches.
The subject of the pattern is the exception that was raised.

```
try do
  raise_error(n)
rescue
  [FunctionClauseError, RuntimeError] ->
    IO.puts "no function match or runtime error"
  error in [ArithmeticError] ->
    IO.inspect error
    IO.puts "Arithmetic error"
    reraise "too late, we're doomed", System.stacktrace
  other_errors ->
    IO.puts "Disaster! #{inspect other_errors}"
after
  IO.puts "DONE!"
end
```

We define three different exception patterns.
The first matches one of the two exceptions, FunctionClauseError or RuntimeError.
The second matches an ArithmeticError and stores the exception value in the variable error.
And the last clause catches any exception into the variable other_error.

_Но эти паттерны отличаются по синтаксу от паттернов case. Кстати, тут нет примера, как матчится на один тип исключения._

We also include an after clause.
This will always run at the end of the try function, regardless of whether an exception was raised.

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

struct, а где-то было написано, что record
TODO разобраться, какая структура данных лежит под exception


## throw, try..catch

A second kind of error are generated when a process calls error, exit, or throw.
All three take a parameter, which is available to the catch handler.

_Я бы сказал, что raise..rescure -- это чисто Эликсировские штуки. А throw..catch унаследованы от Эрланга. И без этого было сложно, но вот, добавили._

У некоторых авторов throw предлагается как control flow. В отличие от raise, который не должен использоваться как control flow.

```
try do
  incite(n)
catch
  :exit, code -> "Exited with code #{inspect code}"
  :throw, value -> "throw called with #{inspect value}"
  what, value -> "Caught #{inspect what} with #{inspect value}"
end
```

_А вот уже третий синтаксис для паттернов._

The purpose of throws if to allow nonlocal returns.
There is no constracts such as break, continue, and return.
But using throws for control flow is hacky. You should avoid this technique.


## Custom exceptions

Exceptions in Elixir are basically records.
_Но records не описаны в книге Дейва Томаса._
все-таки struct?

MatchError is too generic; it can
happen for several reasons. It’s better to provide specific error structs to
clarify the problem by adding more context

You can define your own exceptions by creating a module.
Inside it, use defexception to define the various fields in the exception,
along with their default values.

Because you’re creating a module, you can also add functions —
often these are used to format the exception’s fields into meaningful messages.

```
defmodule KinectProtocolError do
  defexception message: "Kinect protocol error",
    can_retry: false

  def full_message(me) do
    "Kinect failed: #{me.message}, retriable: #{me.can_retry}"
  end
end
```

```
try do
  talk_to_kinect()
rescue
  error in [KinectProtocolError] ->
    IO.puts KinectProtocolError.full_message(error)
    if error.can_retry, do: schedule_retry()
end
```
