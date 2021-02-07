# Pattern Matching

TODO sketch

Образ: розетка и вилка. Для case -- несколько разных розеток и одна вилка.
Найти картинки/фотки в инете, как выглядят разные реальные розетки и вилки, нарисовать.

[pattern] = [value]
Слева:
литералы
несвязанные переменные
связанные переменные (pin operator)

Справа:
литералы
связанные переменные

Обе части могут быть сложной структурой любой вложенности.

====================

Итак, сопоставление с образцом используется для:
- присвоения значений переменным;
- извлечения значений из сложных структур данных;
- условных переходов.


### Присвоение значений переменным

```
1> A = 123.
123
```

Даже эта элементарная конструкция, которая выглядит как оператор
присваивания, на самом деле является сопоставлением с образцом. А
оператора присваивания в эрланг нет вообще.

Переменные в эрланг могут быть несвязанные (unbound) и связанные
(bound).  Несвязанная переменная объявлена, но еще не получила никакого
значения.  Связанная переменная уже получила значение, и теперь не
может его изменить.

В данном коде несвязанная переменная **А**, с помощью сопоставления с
образцом получает значение **123**, и становится связанной.

The equals sign is not an assignment. Instead it’s like an assertion. 
It succeeds if Elixir can find a way of making the left-hand side (pattern) equal the right-hand side (expression). 
Elixir calls the = symbol the **match operator**. 


### Извлечение значений из сложных структур данных

```
2> User = {user, "Bob", 25}.
{user,"Bob",25}
3> {user, Name, Age} = User.
{user,"Bob",25}
4> Name.
"Bob"
5> Age.
25
```

Это мы уже делали на предыдущих уроках, сейчас разберем подробнее, что
здесь происходит.  Слева от знака **=** находится шаблон (pattern),
справа значение, которое мы пытаемся сопоставить с шаблоном.

Шаблон может быть любой структурой данных и может содержать
несвязанные и связанные переменные.  Значение справа может быть любой
структурой данных, но может содержать только связанные переменные.

Сопоставление может пройти успешно, и тогда несвязанные переменные в
шаблоне (если они есть), получат свои значения. Или сопоставление
может не пройти, и тогда генерируется исключение -- ошибка времени
выполнения.

```
6> {cat, Name, TailLength} = User.
** exception error: no match of right hand side value {user,"Bob",25}
```

Шаблон может также содержать анонимные переменные (обозначаются
символом подчеркивания), которые совпадают с любым значением.

```
8> {_, Name, _} = User.
{user,"Bob",25}
9> Name.
"Bob"
```

Но их нужно отличать от именованных переменных, чьи имена начинаются
с символа подчеркивания:

```
10> {_Some, Name, _Some} = User.
** exception error: no match of right hand side value {user,"Bob",25}
```

Здесь первое и третье значения кортежа должны быть одинаковыми, чтобы
шаблон совпал.  Но в значении **User** они разные, поэтому получаем исключение.

Понятие связанной и не связанной переменной. Bound / Unbound.
Есть ли в Эликсир такая терминология? Тут мешается повторное присваивание и pin operator.
Даже если в Эликсире нет такой терминологии, она есть в ФП, так что надо вводить.

```
list = [ 1, 2, 3 ]
[a, b, c ] = list
iex> a
1
```

A pattern (the left side) is matched if the values (the right side) have the same structure 
and if each term in the pattern can be matched to the corresponding term in the values. 
A literal value in the pattern matches that exact value, 
and a variable in the pattern matches by taking on the corresponding value.

Ignoring a Value with _ (anonymouse variable)
it is like a wildcard saying, “I’ll accept any value here.”

Variables Bind Once (per Match)
```
iex> [a, a] = [1, 1]
[1, 1]
iex> a
1
iex> [b, b] = [1, 2]
** (MatchError) no match of right hand side value: [1, 2]
```

However, a variable can be bound to a new value in a subsequent match, 
and its current value does not participate in the new match.

pin operator
Prefix variable with ^ (a caret)
```
iex> a = 1
1
iex> [^a, 2, 3 ] = [ 1, 2, 3 ]
# use existing value of a
[1, 2, 3]
```

Elixir’s pattern matching is similar to Erlang’s 
(the main difference being that Elixir allows a match to reassign to a variable that was assigned in a prior match, 
whereas in Erlang a variable can be assigned only once).


Joe Armstrong, Erlang’s creator, compares the equals sign in Erlang to that used in algebra. 
When you write the equation x = a + 1, you are not assigning the value of a + 1 to x. 
Instead you’re simply asserting that the expressions x and a + 1 have the same value. 
If you know the value of x, you can work out the value of a, and vice versa.
His point is that you had to unlearn the algebraic meaning of = 
when you first came across assignment in imperative programming languages. 
Now’s the time to un-unlearn it.
_это хорошо, это надо взять_

patterns can be arbitrarily nested
```
[_, {name, _}, _] = [{"Bob", 25}, {"Alice", 30}, {"John", 35}]
```

What happens here:
- The expression on the right side is evaluated
- The resulting value is matched against the left-side pattern
- If the match succeeds:
  - Variables from pattern are bound
  - The result of the match expression is the result of the rigth-side term
- If the match fails:
  - MatchError is raised
  

### Условные переходы

```
6> case User of
6> {user, _, _} -> "this is user";
6> {cat, _, _} -> "this is cat"
6> end.
"this is user"
```

Сопоставление с образцом также используется в клозах (clause) функций
и в конструкциях **case**, **receive**, **try** для выбора ветки кода,
которая будет выполняться. То есть, для условного перехода.

Ниже мы рассмотрим все эти варианты. А сейчас один пример из реального
проекта. Это игра, где несколько пользователей собираются за одним
столом. Один из игроков является владельцем комнаты.  Данная функция
позволяет определить, является ли данный игрок владельцем данной
комнаты:

```
is_user_owner_of_room(UserId, RoomId) ->
    case rooms:find_room(RoomId) of
        {ok, #room{owner = UserId}} -> true;
        _ -> false
    end.
```

Здесь **rooms:find_room/1** может вернуть либо **{ok, #room{}}**, либо
**{error, not_found}**. В первом шаблоне конструкции **case** мы
проверяем, что find_room вернула **{ok, #room{}}**, причем owner
совпадает с UserId.

Таким образом, мы одним шаблоном проверяем сразу два условия:

 - что комната с таким RoomId существует;
 - что владелец у нее именно UserId, а не кто-то другой.

В императивном языке тут было бы две конструкции **if**.


## clause

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


## guards

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
