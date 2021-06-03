# Исключения

## raise 

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

Показать примеры:
- failed pattern matching
- invalid arithmetic (division by zero, 1 + :a)
- invocation of non-existing function


## rescue

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


