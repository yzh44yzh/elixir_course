# Map Samples

_Это было в теме 07-02-map, но я убрал оттуда. Не ложится на основную тему: MyCalendar и MyCalendar.Model.Map. Полностью удалять из курса тоже не хочется. Пусть лежит в отдельном файле._


## split, take, drop

Эта группа функций не самая популярная, но полезная в некоторых случаях. Работа с нашим event не такой случай, тут нужен другой пример.

Допустим, у нас есть map с населением крупных городов Европы:

```elixir-iex
iex(1)> population = EuropeCity.population()
%{
  "Barcelona" => 1636762,
  "Belgrade" => 1397939,
  "Berlin" => 3748148,
  ...
```

И список городов, находящихся в странах-членах Евросоюза:

```elixir-iex
iex(2)> eu_cities = EuropeCity.eu_cities()
["Berlin", "Madrid", "Rome", ...
```

С помощью Map.take мы можем выбрать информацию по населению только тех городов, которые входят в Евросоюз:

```elixir-iex
iex(3)> Map.take(population, eu_cities)
```

Функция принимает map и список ключей, отбрасывает из map ключи, не входящие в список, и возвращает новую map. Если в списке есть ключи, которых нет в map, то они просто игнорируются.

Map.drop работает противоположным образом, возвращает map с ключами, не входящими в список.

```elixir-iex
iex(4)> Map.drop(population, eu_cities)
```

То есть мы получили население городов, не входящих в Евросоюз.

Функция Map.split объединяет take и drop. Она разделяет map на две новые map. Первая содержит ключи из списка, вторая не содержит.

```elixir-iex
iex(5)> Map.split(population, eu_cities)
```

## merge

Мы разобрались, как разделить map на две. Теперь посмотрим на противоположную операцию -- объединение двух map в одну.

Для примера возьмем любимую музыку, мою:

```elixir-iex
iex(1)> yura_fs = BestSong.yura_favorite_songs()
%{
  "Bel Suono" => "Libertango",
  "Péter Bence" => "Despacito",
  "The Manhattan Transfer" => "The Offbeat of Avenues"
}
```

и кота Тихона:

```elixir-iex
iex(2)> tihon_fs = BestSong.tihon_favorite_songs()
%{
  "Electric Light Orchestra" => "Mr. Blue Sky",
  "Péter Bence" => "Africa",
  "The Manhattan Transfer" => "The Junction"
}
```

Как видно, наши вкусы частично пересекаются. Мы имеем две map по три ключа в каждой, причем один ключ уникальный, а два конфликтуют.

Посмотрим, что будет, если применить функцию `Map.merge`:

```elixir-iex
iex(3)> Map.merge(yura_fs, tihon_fs)
%{
  "Bel Suono" => "Libertango",
  "Electric Light Orchestra" => "Mr. Blue Sky",
  "Péter Bence" => "Africa",
  "The Manhattan Transfer" => "The Junction"
}
iex(4)> Map.merge(tihon_fs, yura_fs)
%{
  "Bel Suono" => "Libertango",
  "Electric Light Orchestra" => "Mr. Blue Sky",
  "Péter Bence" => "Despacito",
  "The Manhattan Transfer" => "The Offbeat of Avenues"
}
```

Функция объединяет две map в одну. И если есть конфликтующие ключи, то значения для них берутся из map, которая передана вторым аргументом.

Это не всегда подходящая стратегия разрешения конфликтов. Если у нас есть стратегия лучше, то мы можем воспользоваться `Map.merge/3`, где третим аргументом передается функция, разрешающая конфликт:

```elixir-iex
Map.merge(yura_fs, tihon_fs, fn (_artist, song_1, song_2) -> [song_1, song_2] end)
%{
  "Bel Suono" => "Libertango",
  "Electric Light Orchestra" => "Mr. Blue Sky",
  "Péter Bence" => ["Despacito", "Africa"],
  "The Manhattan Transfer" => ["The Offbeat of Avenues", "The Junction"]
}
```

Как видно, функция принимает ключ и оба конфликтующих значения и должна вернуть новое значение.

## map, reduce, filter

Напоследок вспомним, что все функции модуля Enum, которые мы изучали в 5-м уроке, работают с map.

```elixir-iex
iex(10)> Enum.map(yura_fs, fn ({artist, _song}) -> artist end)
["Bel Suono", "Péter Bence", "The Manhattan Transfer"]

iex(14)> Enum.filter(population, fn({_, p}) -> p > 5_000_000 end)
[
  {"Istanbul", 15462452},
  {"London", 9126366},
  {"Moscow", 12195221},
  {"Saint Petersburg", 5383890}
]

iex(17)> Enum.reduce(population, 0, fn({city, p}, acc) ->
...(17)> if city in eu_cities, do: acc + p, else: acc
...(17)> end)
25684115
```

Это работает потому, что модуль Enum поддерживает любые структуры данных, реализующие протокол Enumerable. Наша следущая тема -- **протокол**. Разберемся, что это такое.
