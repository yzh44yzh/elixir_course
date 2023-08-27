# Функции высшего порядка

Функции во всех функциональных языках являются обычными значениями. Их можно сохранять в переменную, передавать в аргументах, возвращать из функции.

Функции высшего порядка (higher-order functions, HOF) -- это функции, которые принимают в аргументах другие функции или возвращают другие функции.

В стандартном модуле **Enum** таких функций много. И самые важные из них, это **map**, **filter** и **reduce**. В других ФП языках (например, в Эрланг) reduce часто называют **fold**. Эти три функции составляют основу применения HOF, потому что все остальное строится на их базе.

Сами эти функции реализованы, как рекурсия с аккумулятором, и их не сложно реализовать самому.

Порядок аргументов таков, чтобы функции было удобно использовать с оператором pipe. Поэтому список (или другая коллекция) всегда идет первым аргументом.

## Map

**map** применяет переданную функцию к каждому элементу списка и возвращает новый список.

```elixir-iex
> lst = [1, 2, 3, 4, 5]
[1, 2, 3, 4, 5]
> f = fn i -> i * i end
#Function<44.97283095/1 in :erl_eval.expr/5>
> Enum.map(lst, f)
[1, 4, 9, 16, 25]
```

```elixir-iex
$ iex lib/hof.exs
> lst = HOF.test_data
[
  {:user, 1, "Bob", 23},
  {:user, 2, "Helen", 20},
  {:user, 3, "Bill", 15},
  {:user, 4, "Kate", 14}
]
> f = fn({:user, id, name, age}) -> {:user, id, String.upcase(name), age} end
#Function<44.97283095/1 in :erl_eval.expr/5>
> Enum.map(lst, f)
[
  {:user, 1, "BOB", 23},
  {:user, 2, "HELEN", 20},
  {:user, 3, "BILL", 15},
  {:user, 4, "KATE", 14}
```

## Filter

**filter** использует переданную функцию, как предикат для фильтрации списка.

```elixir-iex
> f = fn(i) -> i > 3 end
#Function<44.97283095/1 in :erl_eval.expr/5>
> Enum.filter([1, 2, 3, 4, 5], f)
[4, 5]
```

```elixir-iex
> users = HOF.test_data
[
  {:user, 1, "Bob", 23},
  {:user, 2, "Helen", 20},
  {:user, 3, "Bill", 15},
  {:user, 4, "Kate", 14}
]
> f = fn({:user, id, _, _}) -> rem(id, 2) == 0 end
#Function<44.97283095/1 in :erl_eval.expr/5>
> Enum.filter(users, f)
[{:user, 2, "Helen", 20}, {:user, 4, "Kate", 14}]
```

## Примеры

Мы можем взять примеры из прошлого урока и переписать их с использованием map и filter.

Отфильтровать совершеннолетних пользователей:

```elixir-iex
> users = HOF.test_data
[
  {:user, 1, "Bob", 23},
  {:user, 2, "Helen", 20},
  {:user, 3, "Bill", 15},
  {:user, 4, "Kate", 14}
]
> Enum.filter(users, fn({:user, _, _, age}) -> age > 16 end)
[{:user, 1, "Bob", 23}, {:user, 2, "Helen", 20}]
```

Получить id и name пользователя:

```elixir-iex
> Enum.map(users, fn({:user, id, name, _}) -> {id, name} end)
[{1, "Bob"}, {2, "Helen"}, {3, "Bill"}, {4, "Kate"}]
```

Разделить пользователей на два списка: несовершеннолетние и взрослые:

```elixir-iex
> adults = Enum.filter(users, fn({:user, _, _, age}) -> age > 16 end)
[{:user, 1, "Bob", 23}, {:user, 2, "Helen", 20}]
> children = Enum.filter(users, fn({:user, _, _, age}) -> age <= 16 end)
[{:user, 3, "Bill", 15}, {:user, 4, "Kate", 14}]
```

Здесь мы сделали два прохода по списку. Разделение в один проход сделаем немного позже с помощью Enum.reduce.
