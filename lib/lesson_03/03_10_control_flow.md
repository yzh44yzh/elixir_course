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

case могут быть вложенными друг в друга: 
```
  def handle(animal, action) do
    case animal do
      {:dog, name} ->
        case action do
          :add -> IO.puts("add dog #{name}")
          :remove -> IO.puts("remove dog #{name}")
        end
      {:cat, name} ->
        case action do
          :add -> IO.puts("add cat #{name}")
          :remove -> IO.puts("remove cat #{name}")
        end
    end
  end
```

Вложенный даже на два уровня код плохо читается. Обычно этого можно избежать. Данный пример можно реализовать без вложенного case таким образом:
```
  def handle2(animal, action) do
    case {animal, action} do
      {{:dog, name}, :add} -> IO.puts("add dog #{name}")
      {{:dog, name}, :remove} -> IO.puts("remove dog #{name}")
      {{:cat, name}, :add} -> IO.puts("add cat #{name}")
      {{:cat, name}, :remove} -> IO.puts("remove cat #{name}")
    end
  end
```


## Клозы функций (Clause)

Вторая по популярности конструкция (или даже первая, в зависимости от предпочтений разработчика), это использование клозов функций. 

Этот термин пишется **clause**, произносится **[klôz]** и означает одно из нескольких тел функции.

В Эликсир одна функция может иметь несколько тел -- несколько разных блоков кода. В зависимости от входящих аргументов выполняется только один из этих блоков.

```
  def handle3({:dog, name}, :add) do
    IO.puts("add dog #{name}")
  end
  def handle3({:dog, name}, :remove) do
    IO.puts("remove dog #{name}")
  end
  def handle3({:cat, name}, :add) do
    IO.puts("add cat #{name}")
  end
  def handle3({:cat, name}, :remove) do
    IO.puts("remove cat #{name}")
  end
```

Шаблоны описываются прямо в аргументах функции, отдельно для каждого тела. Принцип такой же, как и с конструкцией **case** -- шаблоны проверяются по очереди на совпадение с входящими аргументами функции. Первый совпавший шаблон вызывает соответствующий блок кода и останавливает дальшейший перебор. Если ни один шаблон не совпал, то генерируется исключение.

Как и в случае с **case**, здесь тоже важна очередность шаблонов. Типичная ошибка -- расположить более общий шаблон выше, чем более специфичный шаблон:
```
  def handle3(animal, action) do
    IO.puts("do something")
  end
  def handle3({:dog, name}, :add) do
    IO.puts("add dog #{name}")
  end

``` 
Во многих таких случаях компилятор выдаст предупреждение:
```
warning: this clause for handle3/2 cannot match because a previous clause at line 27 always matches
  lib/lesson_03/task_03_10_control_flow.exs:30
```
Но бывает, что компилятор не замечает проблему:
```
  def handle3(animal, :add) do
```


## Охранные выражения (Guards)

Теперь вернемся к упомянутым выше охранным выражениям. 

Не всегда достаточно шаблона, чтобы проверить все условия для ветвления в коде. Например, шаблоном нельзя проверить попадание числа в определенный диапазон.

```
  def handle4(animal) do
    case animal do
      {:dog, name, age} when age > 10 -> IO.puts("#{name} is a dog older than 10")
      {:dog, name, _} -> IO.puts("#{name} is a 10 years old or younger dog")
      {:cat, name, age} when age > 10 -> IO.puts("#{name} is a cat older than 10")
      {:cat, name, _} -> IO.puts("#{name} is a 10 years old or younger cat")
    end
  end
```

Охранные выражения могут использоваться и с **case**, и с телами функций:
```
  def handle5({:dog, name, age}) when age > 10 do
    IO.puts("#{name} is a dog older than 10")
  end
  def handle5({:dog, name, _age}) do
    IO.puts("#{name} is a 10 years old or younger dog")
  end
  def handle5({:cat, name, age}) when age > 10 do
    IO.puts("#{name} is a cat older than 10")
  end
  def handle5({:cat, name, _age}) do
    IO.puts("#{name} is a 10 years old or younger cat")
  end
```

TODO stopped here


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
