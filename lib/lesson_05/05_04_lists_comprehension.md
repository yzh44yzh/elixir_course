## Конструкторы списков

Конструкторы списков (lists comprehension) -- еще один высокоуровневый способ работы с коллекциями.

Они позволяют делать многое из того, что делается рассмотренными раньше функциями map и filter, но в более лаконичном синтаксисе.
Comprehensions generally provide a much more concise representation than using the equivalent functions from the Enum and Stream modules.

Пример map:
```
iex(12)> users
[
  {:user, 1, "Bob", 23},
  {:user, 2, "Helen", 20},
  {:user, 3, "Bill", 15},
  {:user, 4, "Kate", 14}
]
iex(13)> for {:user, id, name, _age} <- users, do: {id, name}
[{1, "Bob"}, {2, "Helen"}, {3, "Bill"}, {4, "Kate"}]
```

Пример filter:
```
iex(14)> for {:user, _id, _name, age} = user <- users, age > 16, do: user
[{:user, 1, "Bob", 23}, {:user, 2, "Helen", 20}]
```

Конструкторы списков позволяют объединять map и filter в одном проходе:
```
iex(16)> for {:user, _id, name, age} <- users, age > 16, do: name         
["Bob", "Helen"]
```

The idea of a comprehension is fairly simple: 
given one or more collections,
extract all combinations of values from each, 
optionally filter the values, 
and then generate a new collection using the values that remain.

The general syntax for comprehensions is deceptively simple:
```
result = for generator or filter... [ , into: value ] , do: expression
```

In the expression above, n <- [1, 2, 3, 4] is the generator. It is literally generating values to be used in the comprehension. Any enumerable can be passed on the right-hand side of the generator expression.

Generator expressions also support pattern matching on their left-hand side; all non-matching patterns are ignored. (TODO example)

Alternatively to pattern matching, filters can be used to select some particular elements.
Comprehensions discard all elements for which the filter expression returns false or nil; all other values are selected.

The general syntax for comprehensions is deceptively simple:
result = for generator or filter... [ , into: value ] , do: expression

A generator specifies how you want to extract values from a collection.
pattern <- enumerable_thing

A filter is a predicate. It acts as a gatekeeper for the rest of the comprehen-
sion—if the condition is false, then the comprehension moves on to the next
iteration without generating an output value.

Конструкторы списков позволяют обрабатывать несколько списков одновременно:

```
iex(21)> for x <- list1, y <- list2, z <- list3, do: {x, y, z}
[
  {1, :a, "hello"},
  ...
  {4, :c, "world"}
]

```

Как видно, элементы списков соединяются "каждый с каждым".

Ну и, конечно, все эти списки можно фильтровать:

```
iex(22)> for x <- list1, y <- list2, z <- list3, x > 2, y != :b, do: {x, y, z}
[
  {3, :a, "hello"},
  {3, :a, "world"},
  {3, :c, "hello"},
  {3, :c, "world"},
  {4, :a, "hello"},
  {4, :a, "world"},
  {4, :c, "hello"},
  {4, :c, "world"}
]
```

TODO таблица умножения
for x <- 1..9, y <- 1..9, do: {x, y, x * y}


## Пифагоровы тройки

Ну и напоследок красивый пример из книги Джо Армстронга с пифагоровыми тройками.  Вспомним теорему Пифагора: сумма квадратов катетов равна квадрату гипотенузы.  Существует не так много вариантов, когда длины катетов и гипотенузы выражаются целыми числами. Самый известный такой вариант: {3, 4, 5}.

Армстронг предлагает найти все такие варианты с помощью конструкторов списков.  На входе дана максимальная длина гипотенузы, на выходе нужно получить список всех возможных троек **{Катет, Катет, Гипотенуза}**, где длины являются целыми числами.

Берем список всех возможных длин:

```
iex(23)> max_length = 20
20
iex(24)> lengthes = Enum.to_list(1..max_length)
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
```

И генерируем все возможные сочетания длин:

```
iex(26)> for x <- lengthes, y <- lengthes, z <- lengthes, do: {x, y, z}
[
  {1, 1, 1},
 ...
```

Промежуточный результат получится очень большой, но это не важно. Дальше его нужно отфильтровать.

```
iex(27)> for x <- lengthes, y <- lengthes, z <- lengthes, x * x + y * y == z * z, do: {x, y, z}
[
  {3, 4, 5},
  {4, 3, 5},
  {5, 12, 13},
  {6, 8, 10},
  {8, 6, 10},
  {8, 15, 17},
  {9, 12, 15},
  {12, 5, 13},
  {12, 9, 15},
  {12, 16, 20},
  {15, 8, 17},
  {16, 12, 20}
]
```

Хорошо бы еще устранить дубликаты. Для этого добавим еще один фильтр.

```
iex(29)> for x <- lengthes, y <- lengthes, z <- lengthes,
...(29)> x < y,
...(29)> x * x + y * y == z * z, 
...(29)> do: {x, y, z}
[{3, 4, 5}, {5, 12, 13}, {6, 8, 10}, {8, 15, 17}, {9, 12, 15}, {12, 16, 20}]
```

Задача решена одной строкой кода :)