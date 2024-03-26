# Сопоставление с образцом для словарей (map)

Есть некоторые нюансы сопоставления с образцом при работе со словарями. В шаблоне не нужно перечислять все ключи, какие есть в словаре. Мы указываем только те ключи, которые нам нужны:

```elixir-iex
iex(4)> my_map = %{a: 1, b: 2, c: 3}
%{a: 1, b: 2, c: 3}
iex(5)> %{a: value} = my_map
%{a: 1, b: 2, c: 3}
iex(6)> value
1
```

Можно извлечь несколько значений одновременно:

```elixir-iex
iex(2)> %{a: a_val, b: b_val} = my_map
%{a: 1, b: 2, c: 3}
iex(3)> a_val
1
iex(4)> b_val
2
```

Если в словаре нет нужного ключа, то возникает исключение `MatchError`:

```elixir-iex
iex(7)> %{d: val} = my_map
** (MatchError) no match of right hand side value: %{a: 1, b: 2, c: 3}
```

Если ключи не являются атомами, то синтаксис отличается:

```elixir-iex
iex(7)> my_map = %{"a" => 1, "b" => 2, "c" => 3}
%{"a" => 1, "b" => 2, "c" => 3}
iex(8)> %{"a" => value1} = my_map
%{"a" => 1, "b" => 2, "c" => 3}
iex(9)> %{"a" => value1, "b" => value2} = my_map
%{"a" => 1, "b" => 2, "c" => 3}
iex(10)> value1
1
iex(11)> value2
2
```

Шаблон `%{}` совпадает с любым словарём. Это контринтуитивно, можно было бы ожидать, что этот шаблон совпадает только с пустым словарём. Этим шаблоном нельзя ничего извлечь, но можно проверить, что значение является словарём, а не чем-то иным.

```elixir-iex
iex(13)> %{} = my_map
%{"a" => 1, "b" => 2, "c" => 3}
```


## Использование переменных

Переменные можно использовать для извлечения значений, но не для извлечения ключей:

```elixir-iex
iex(15)> %{"c" => my_var} = my_map
%{"a" => 1, "b" => 2, "c" => 3}
iex(16)> my_var
3
iex(17)> %{my_var => 1} = my_map
** (CompileError) iex:17: cannot use variable my_var as map key inside a pattern.
```

Это очевидно, так как значения не уникальны, и если есть несколько одинаковых значений, то непонятно, какой именно нужен ключ.

А _pin_ operator можно использовать и для ключа, и для значения:

```elixir-iex
iex(18)> value1
1
iex(19)> %{"a" => ^value1} = my_map
%{"a" => 1, "b" => 2, "c" => 3}
iex(20)> keyb = "b"
"b"
iex(21)> %{^keyb => _} = my_map
%{"a" => 1, "b" => 2, "c" => 3
```

Вариант `%{"a" => ^value1} = my_map` эквивалентен `%{"a" => 1} = my_map`. То есть, такой шаблон проверяет, что в словаре есть ключ и именно с таким значением.


## Извлечение значений из вложенных словарей

Работает так же, как для кортежей и списков.

Создадим структуру из вложенных словарей и списков:

```
iex(1)> book1 = %{id: 1, title: "Little Ecto Book"}
%{id: 1, title: "Little Ecto Book"}
iex(2)> book2 = %{id: 2, title: "Programming Ecto"}
%{id: 2, title: "Programming Ecto"}
iex(3)> book3 = %{id: 3, title: "Programming Phoenix"}
%{id: 3, title: "Programming Phoenix"}
iex(4)> library = %{topic: "Elixir", books: [book1, book2, book3]}
%{
  topic: "Elixir",
  books: [
    %{id: 1, title: "Little Ecto Book"},
    %{id: 2, title: "Programming Ecto"},
    %{id: 3, title: "Programming Phoenix"}
  ]
}
```

Извлечём название 1й книги:

```
iex(7)> %{books: [%{title: title} | _]} = library
iex(8)> title
"Little Ecto Book"
```

Извлечём id 2й книги:

```
iex(9)> %{books: [_, %{id: id}, _]} = library
iex(10)> id
2
```

Извлечём id всех книг:

```
iex(11)> %{books: [%{id: id1}, %{id: id2}, %{id: id3}]} = library
iex(12)> {id1, id2, id3}
{1, 2, 3}
```

## Использование get_in

В Эликсир есть набор функций и макросов для работы с вложенным структурами данных: `get_in`, `put_in`, `update_in`. Эта тема не относится к сопоставлению с образом, и мы рассмотрим эти функции позже, в 7-м уроке.

Но раз уж у нас под руками есть подходящая структура, то я покажу `get_in`.

```
iex(5)> get_in(library, [:books])
[
  %{id: 1, title: "Little Ecto Book"},
  %{id: 2, title: "Programming Ecto"},
  %{id: 3, title: "Programming Phoenix"}
]

iex(6)> get_in(library, [:books, Access.all(), :title])
["Little Ecto Book", "Programming Ecto", "Programming Phoenix"]

iex(7)> get_in(library, [:books, Access.at(0), :title])
"Little Ecto Book"

iex(8)> get_in(library, [:books, Access.at(1), :title])
"Programming Ecto"
```

Это как в длинных сериалах открывают новые сюжетные линии, чтобы зритель не терял интерес и хотел посмотреть следущий сезон :)
