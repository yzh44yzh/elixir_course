# Control Flow

## Control flow

Small functions and a combination of guard clauses and pattern matching of parameters 
replaces most of the control flow.
Functions written without explicit control flow tend to be shorter and more focused.
However, Elixir does have a small set of control-flow constructs.


## do-end block, do: form

Way of grouping expressions and passing them to other code:
- module definitions
- function definitions
- control structures
- any place in Elixir where code needs to be handled as an entity.

do-end block is syntax sugar:
```
def double(n) do
  n * 2
end
```

The actual syntax do: form:
```
def double(n), do: n * 2
```

You can pass multiple lines by grouping them with parentheses:
```
def double(n), do: (
  IO.puts(n)
  n * 2
)
```
And the do: form itself is nothing special; it is simply a term in a keyword list.

Typically people use the do: form for single-line blocks, 
and do-end block for multiline ones.

```
defmodule Greeter do

  def hello(name) do
    "Hello " <> name <> "!"
  end

end
```

Same:
```
defmodule Greeter, do: (
  def hello(name), do: (
    "Hello " <> name <> "!"
  )
)
```


## if expression

```
if false, do: :this, else: :that
syntax sugar for:macros
if(false, [do: :this, else: :that])
syntax sugar for:
if(false, [{:do, :this}, {:else, :that}])
```

unless is similar:
```
unless 1 == 2, do: "OK", else: "error"

unless 1 == 2 do
  "OK"
else
  "error"
end
```

The if expression returns the result of the executed block. If the condition isn't met and the else clause isn't specified, the return value is the atom nil.


## cond

```
cond do
  rem(current, 3) == 0 and rem(current, 5) == 0 ->
    "FizzBuzz"
  rem(current, 3) == 0 ->
    "Fizz"
  rem(current, 5) == 0 ->
    "Buzz"
  true ->
    current
end
```


Конструкция **if** представляет собой упрощенный **case** без выражения и
без шаблонов, а ветки представлены только гардами.

```
if
    GuardSeq1 ->
        Body1;
    ...;
    GuardSeqN ->
        BodyN
end
```

Здесь, как и с case, если ни один гард не сработал, генерируется
исключение.  Довольно часто бывает, что последним гардом ставят true,
и он срабатывает всегда.

```
valid_char(Char) ->
    IsDigit = is_digit(Char),
    IsAlpha = is_alpha(Char),
    if
        IsDigit -> true;
        IsAlpha -> true;
        true -> false
    end.
```

Еще в этом примере мы видим, как обойти ограничение на использование
своих функций в гардах. Мы просто вызываем эти функции за пределами
if, присваиваем результат в переменные, и уже переменные используем в
гардах.

Для case тоже можно делать последний шаблон, который обязательно
совпадет с любым выражением (catch all pattern).  Но так делают реже,
чем в случае с if. А почему, мы выясним на одном из следующих уроков,
когда будем изучать обработку ошибок и принцип **Let It Crash**.


## case

```
case File.open("case.ex") do
  {:ok, file} ->
    IO.puts "First line: #{IO.read(file, :line)}"
  {:error, reason} ->
    IO.puts "Failed to open file: #{reason}"
end
```

Конструкция **case** аналогична клозам функции, но может
использоваться в любом месте в коде.

```
case Expr of
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end
```

Как и с клозами, шаблоны применяются к выражению по очереди, сверху
вниз.  Первый совпавший шаблон определяет ветку кода, которая будет
выполняться. Если ни один шаблон не совпал, то генерируется
исключение.

case могут быть вложенными друг в друга. 2 уровня вложенности
допустимы.  3 уровня читаются (и пишутся) плохо, этого лучше избегать.

Вот пример на 2 уровня вложенности:

```
close_room(UserId, RoomId) ->
    case rooms:find_room(RoomId) of
        {ok, #room{owner = UserId}} ->
            case rooms:close(RoomId) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {ok, #room{}} -> {error, not_room_owner};
        {error, not_found} -> {error, room_not_found}
    end.
```

Здесь пользователь пытается закрыть комнату. Результатом этой попытки
может быть успешное закрытие комнаты, или несколько вариантов ошибок:
пользователь не является владельцем комнаты, комната не найдена и
т.д. Эти варианты обрабатываются двумя case.

Если мы создаем новую переменную в одной из веток case, и потом
пытаемся ее использовать за пределами case, то такой код не
скомпилируется.  Переменная считается небезопасной (unsafe), потому
что может оказаться не определена.  Правильный вариант -- не
использовать переменную за пределами ветки case, в которой она
объявлена, либо объявить во всех ветках.
