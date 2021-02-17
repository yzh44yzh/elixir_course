# Условные переходы (Control Flow)

Условные переходы в функциональных языках сильно отличаются от императивных, потому что основаны на сопоставлении с образцом. Основная идея в том, что некое значение по-очереди сравнивается с несколькими шаблонами, и в зависимости от того, с каким шаблоном оно совпадет, выполняется та или иная ветка кода.

Есть несколько вариантов условных переходов:
- конструкция case
- клозы функций (clause)
- обработка исключений (resque, catch)
- чтение сообщений из mailbox (receive) 

Все они реализуют эту идею. 


## Конструкция case

Начнем с примеров, которые мы уже видели:
```
def gcd(a, b) do
  case rem(a, b) do
    0 -> b
    c -> gcd(b, c)
  end
end
```
Здесь вычисляется значение **rem(a, b)**, и сравнивается с двумя шаблонами. Первый шаблон -- литерал **0**. Если значение совпадает с ним, то выполняется код, завершающий рекурсию и возвращающий **b**. Второй шаблон -- переменная **c**. С этим шаблоном совпадут любые значения. И выполняется код -- вызов **gcd(b, c)**.

Второй пример:
```
case Map.fetch(acc, word) do
  {:ok, count} -> Map.put(acc, word, count + 1)
  :error -> Map.put(acc, word, 1)
end
```
Здесь выполняется вызов функции **Map.fetch(acc, word)**. Получившееся значение сравнивается с двумя шаблонами и выполняется соответствующий код.

Шаблонов может быть несколько. И важен их порядок, потому что первый совпавший шаблон останавливает перебор оставшихся шаблонов. Если не совпал ни один из шаблонов, то генерируется исключение.

В общем виде конструкция **case** выглядит так: 
```
case Expr do
    Pattern1 [when GuardSequence1] ->
        Body1
    ...
    PatternN [when GuardSequenceN] ->
        BodyN
end
```
Что такое GuardSequence -- цепочка охранных выражений, мы рассмотрим позже.

TODO stopped here

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

Для case тоже можно делать последний шаблон, который обязательно
совпадет с любым выражением (catch all pattern).  Но так делают реже,
чем в случае с if. А почему, мы выясним на одном из следующих уроков,
когда будем изучать обработку ошибок и принцип **Let It Crash**.


## Клозы функций (Clause)

Рассмотрим подробнее клозы функции.  Этот термин пишется **clause**,
произносится **[klôz]** и означает одно из нескольких тел функции.

Общепринятого перевода на русский язык нет, поэтому я буду писать без
перевода -- **клоз**, потому что каждый раз писать "одно из нескольких
тел функции", несколько утомительно :)

Примеры мы видели, когда писали рекурсивные функции с аккумуляторами.
Вообще клозов у функции может быть много:

```
area({rect, Width, Height}) -> Width * Height;
area({square, Size}) -> Size * Size;
area({circle, Radius}) -> math:pi() * Radius * Radius.
```

Очередность клозов важна, потому что шаблоны проверяются сверху вниз,
и первое совпадение приводит к выполнению соответствующего клоза.
Поэтому более специфичные шаблоны должны идти раньше, а более общие
позже. Компилятор может предупредить о неправильной
последовательности шаблонов, но не всегда.

Вот неправильная последовательность шаблонов:

```
case List of
    [] -> empty_list;
    [Head | _] -> process(Head);
    [{X, Y} | _] -> process(X, Y)
end.
```

Шаблон **Head** более общий, чем **{X, Y}**, и третья вертка кода
никогда не сработает, все перехватит вторая ветка.

Вот правильная последовательность шаблонов:

```
case List of
    [] -> empty_list;
    [{X, Y} | _] -> process(X, Y);
    [Head | _] -> process(Head)
end.
```


## Охранные выражения (Guards)

https://hexdocs.pm/elixir/patterns-and-guards.html#content

Guards are a way to augment pattern matching with more complex checks. They are allowed in a predefined set of constructs where pattern matching is allowed, such as function definitions, case clauses, and others.

Not all expressions are allowed in guard clauses, but only a handful of them. This is a deliberate choice. This way, Elixir (and Erlang) can make sure that nothing bad happens while executing guards and no mutations happen anywhere. It also allows the compiler to optimize the code related to guards efficiently.

https://hexdocs.pm/elixir/patterns-and-guards.html#list-of-allowed-functions-and-operators

Macros constructed out of any combination of the above guards are also valid guards - for example, Integer.is_even/1. 

Guards start with the when operator, followed by a guard expression. The clause will be executed if and only if the guard expression returns true. Multiple boolean conditions can be combined with the and and or operators.

A function clause will be executed if and only if its guard expression evaluates to true. If any other value is returned, the function clause will be skipped. In particular, guards have no concept of "truthy" or "falsey".

In guards, when functions would normally raise exceptions, they cause the guard to fail instead.
that if any function call in a guard raises an exception, the entire guard fails. 


**guard** переводится как "охранное выражение".

Гарды используются там, где сопоставление с образцом применяется для
условных переходов: то есть, в клозах функций, в case, try и receive
конструкциях.  Они дополняют сопоставление с образцом, позволяя
указать дополнительные условия.

Гадром является последовательность выражений, разделенных запятой,
каждое из которых вычисляется в булевое значение.

```
check_user({user, _, Gender, Age}) when Gender =:= female, Age < 14 -> girl;
check_user({user, _, Gender, Age}) when Gender =:= female, Age >= 14, Age < 21 -> teenage_girl;
check_user({user, _, Gender, Age}) when Gender =:= female, Age >= 21 -> woman;
check_user({user, _, Gender, Age}) when Gender =:= male, Age < 14 -> boy;
check_user({user, _, Gender, Age}) when Gender =:= male, Age >= 14, Age < 21 -> teenage_boy;
check_user({user, _, Gender, Age}) when Gender =:= male, Age >= 21 -> man.
```

Гард срабатывает (разрешает выполнение данной ветки кода), если все
выражения вычисляются в true.

Гарды могут объединяться в последовательности, разделенные точкой с запятой:

```
check_user({user, _, Gender, Age})
  when Gender =:= female, Age < 14;
       Gender =:= male, Age < 14
       -> child;
check_user({user, _, Gender, Age})
  when Gender =:= female, Age >= 21;
       Gender =:= male, Age >= 21
       -> adult.
```

Последовательность гардов срабатывает, если срабатывает любой из
гардов в ней.

То есть, запятая работает как **andalso**, а точка с запятой работает
как **orelse**, и код выше эквивалентен коду:

```
check_user({user, _, Gender, Age})
  when (Gender =:= female andalso Age < 14) orelse
       (Gender =:= male andalso Age < 14)
       -> child;
check_user({user, _, Gender, Age})
  when (Gender =:= male andalso Age >= 21) orelse
       (Gender =:= male andalso Age >= 21)
       -> adult.
```


Выражения в гардах не должны иметь побочных эффектов. Поэтому
разрешены не любые эрланговские выражения, а только их
подмножество. Например, запрещен вызов пользовательских функций. Да и
встроенные функции можно вызывать не все.  Что именно разрешено,
[смотрите в документации](http://erlang.org/doc/reference_manual/expressions.html#id81911)

Если при вычислении выражения в гарде возникает исключение, то
оно не распространяется дальше, а просто гард не срабатывает
(данная ветка кода не выполняется).

Keep in mind errors in guards do not leak but simply make the guard fail


## Конструкция cond

case is useful when you need to match against different values. However, in many circumstances, we want to check different conditions and find the first one that does not evaluate to nil or false.

This is equivalent to else if clauses in many imperative languages (although used much less frequently here).

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

If all of the conditions return nil or false, an error (CondClauseError) is raised. For this reason, it may be necessary to add a final condition, equal to true, which will always match.

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


## Конструкция if

В Эликсир она есть, в Эрланг ее нет. Вернее сказать, в Эрланг конструкция if работает точно так же, как cond в Эликсир. А в Эликсир это не настоящая конструкция языка, а макрос. 

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

 An interesting note regarding if/2 and unless/2 is that they are implemented as macros in the language; they aren’t special language constructs as they would be in many languages.


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
