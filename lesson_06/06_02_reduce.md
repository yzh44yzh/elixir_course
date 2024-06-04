# Reduce (Fold)

В разных языках эта функция называется по-разному. Где-то она называется **fold** (свертка), а где-то **reduce** (сокращение). Мне больше нравится название fold, как принято в Эрланг. Но в Эликсир её называют reduce.

Свертка -- важный прием в функциональном программировании. Понять её немного сложнее, чем map и filter. Ну, давайте разберемся.

Map и filter принимают на вход список и возвращают список. Reduce принимает список, а возвращает одно значение, то есть сокращает (сворачивает) список.

**Enum.reduce/3** принимает 3 аргумента:
- коллекция (в данном случае список)
- начальное значение аккумулятора
- функцию сворачивания

Функция сворачивания принимает 2 аргумента: текущий элемент списка и текущее значение аккумулятора. И вернуть она должна новое значение аккумулятора.

Простые книжные примеры -- суммирование и произведение элементов списка:

```elixir-iex
> lst = [1, 2, 3, 4, 5]
[1, 2, 3, 4, 5]
> Enum.reduce(lst, 0, fn(item, acc) -> item + acc end)
15
> Enum.reduce(lst, 0, &+/2)
15
> Enum.reduce(lst, 1, fn(item, acc) -> item * acc end)
120
> Enum.reduce(lst, 1, &*/2)
120
```

Для суммирования начальное значение аккумулятора 0, и потом к нему прибавляется каждый элемент списка. Для произведения начальное значение аккумулятора 1, и потом на него умножается каждый элемент списка.

Но это примеры мне кажутся неудачными, не раскрывают тему. Давайте вернёмся к нашему списку пользователей.


## Пример 1

Вычислим средний возраст всех пользователей в списке.

```
  def get_average_age(users) do
    collect_stat =
      fn({:user, _id, _name, age}, {total_users, total_age}) ->
        {total_users + 1, total_age + age}
      end

    {total_users, total_age} = Enum.reduce(users, {0, 0}, collect_stat)
    total_age / total_users
  end
```

Здесь `collect_stat` -- сворачивающая функция. Она принимает текущего пользователя из списка и аккумулятор. В аккумуляторе мы накапливаем общее количество пользователей и их суммарный возвраст. Функция обновляет аккумулятор с учётом текущего пользователя.

Затем мы вызываем `Enum.reduce` с начальным значением аккумулятора `{0, 0}` и на выходе получаем аккумулятор с учётом всех пользователей.

Запускаем:

```elixir-iex
$ iex lib/hof.exs
> users = HOF.test_data
[
  {:user, 1, "Bob", 23},
  {:user, 2, "Helen", 20},
  {:user, 3, "Bill", 15},
  {:user, 4, "Kate", 14}
]
> HOF.get_average_age(users)
18.0
```

Сворачивающую функцию обычно пишут прямо в аргументах reduce:

```
  def get_average_age(users) do
    {total_users, total_age} =
      Enum.reduce(
        users,
        {0, 0},
        fn({:user, _id, _name, age}, {total_users, total_age}) ->
          {total_users + 1, total_age + age}
      end)

    total_age / total_users
  end
```

Такой код труднее читать -- перед глазами слишком много всего, требующего осмысления. В учебных целях я написал сворачивающую функцию отдельно. Так проще понять код.

Бывает так, что я пишу отдельную сворачивающую функцию и в реальных проектах, если она слишком большая и сложная. Но тогда я делаю её именованной, а не анонимной:

```
  def get_average_age(users) do
    {total_users, total_age} = Enum.reduce(users, {0, 0}, &average_age_reducer/2)
    total_age / total_users
  end

  defp average_age_reducer({:user, _id, _name, age}, {total_users, total_age}) do
    {total_users + 1, total_age + age}
  end
```


## Пример 2

Давайте вернемся к задаче с разделением пользователей на два списка по возрасту.

```
  def split_by_age(users, age) do
    Enum.reduce(
      users,
      {[], []},
      fn {:user, _id, _name, user_age} = user, {older, younger} ->
        if user_age > age do
          {[user | older], younger}
        else
          {older, [user | younger]}
        end
      end
    )
  end
```

В аккумуляторе мы используем два списка, куда и складываем пользователей в зависимости от возраста.

В отличие от предыдущей реализации с `Enum.filter`, здесь мы выполняем один проход по списку.

```elixir-iex
> HOF.split_by_age(users, 20)
{[{:user, 1, "Bob", 23}],
 [{:user, 4, "Kate", 14}, {:user, 3, "Bill", 15}, {:user, 2, "Helen", 20}]}
> HOF.split_by_age(users, 10)
{[
   {:user, 4, "Kate", 14},
   {:user, 3, "Bill", 15},
   {:user, 2, "Helen", 20},
   {:user, 1, "Bob", 23}
 ], []}
```

## Пример 3

Найдем самого старшего пользователя.

В качестве начального значения аккумулятора можно взять любого пользователя, кто уже есть в списке. Удобно взять первого.

```
  def get_longest_name_user(users) do
    [first | rest] = users

    Enum.reduce(
      rest,
      first,
      fn user, acc ->
        {:user, _, name, _} = user
        {:user, _, longest_name, _} = acc
        if String.length(longest_name) < String.length(name) do
          user
        else
          acc
        end
    end)
  end
```

Однако, кроме функции `Enum.reduce/3` есть ещё функция `Enum.reduce/2`, которая именно так и работает -- берёт первый элемент списка как начальное значение аккумулятора:

```
  def get_longest_name_user2(users) do
    Enum.reduce(
      users,
      fn user, acc ->
        {:user, _, name, _} = user
        {:user, _, longest_name, _} = acc
        if String.length(longest_name) < String.length(name) do
          user
        else
          acc
        end
    end)
  end
```

Запускаем:

```elixir-iex
> HOF.get_longest_name_user(users)
{:user, 2, "Helen", 20}
```

**map**, **filter** и **reduce** используются повсеместно, вы увидите их практически в любом проекте. Но на них функции высшего порядка не заканчиваются, так что нам есть ещё что рассмотреть.
