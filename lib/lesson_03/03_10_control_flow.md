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

Охранное выражение представляет собой предикат, или цепочку предикатов:
```
when predicat1 and predicat2 or ... predicatN ->
```

В предикатах можно использовать ограниченный набор функций, описанный в [документации](https://hexdocs.pm/elixir/patterns-and-guards.html#list-of-allowed-functions-and-operators). Некоторые функциональные языки разрешают вызывать любые функции в охранных выражениях. Но Эликсир (и Эрланг) не относятся к таким языкам.

Если при вычислении охранного выражения возникает исключение, то оно не приводит к остановке процесса, а приводит к тому, что все выражение вычисляется в false:
```
  def handle6(num) when 10 / num > 2 do
    IO.puts("clause 1")
  end
  def handle6(num) do
    IO.puts("clause 2")
  end
```

Это позволяет писать выражения проще. Вместо:
```
when is_map(a) and map_size(a) > 10 ->
```
можно сразу писать:
```
when map_size(a) > 10 ->
```


## Конструкция cond

Если из **case** убрать шаблоны, но оставить охранные выражения, то получится конструкция **cond**. 

Было:
```
case Expr do
    Pattern1 [when GuardSequence1] ->
        Body1
    ...
    PatternN [when GuardSequenceN] ->
        BodyN
end
```

Стало:
```
cond do
    GuardSequence1 ->
        Body1
    ...
    GuardSequenceN ->
        BodyN
end
```

Мы использовали эту конструкцию в первом уроке, когда реализовывали FizzBuzz:
```
  def fizzbuzz(n) do
    cond do
      rem(n, 3) == 0 and rem(n, 5) == 0 -> IO.puts("FizzBuzz")
      rem(n, 3) == 0 -> IO.puts("Fizz")
      rem(n, 5) == 0 -> IO.puts("Buzz")
      true -> IO.puts(n)
    end
  end
```

В принципе, это эквивалент цепочки **if...else if**, которая не редко встречается в императивных языках. Python, например:
```
a = int(input())
if a < -5:
    print('Low')
elif -5 <= a <= 5:
    print('Mid')
else:
    print('High')
```

Как и в конструкции **case**, очередность выражений важна, и если ни одно из выражений не вычислилось в true, то возникает исключение.

```
  def handle7(num) do
    cond do
      num > 10 -> IO.puts("more than 10")
      num > 5 -> IO.puts("more than 5")
    end
  end

CF.handle7(20)
CF.handle7(8)
CF.handle7(3)
** (CondClauseError) no cond clause evaluated to a truthy value
```


## Конструкция if

В Эрланг есть конструкция **if**, но она точно такая же, как **cond** в Эликсир. 

TODO stopped here

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
