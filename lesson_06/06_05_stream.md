# Модуль Stream

## Ленивые вычисления

Начать стоит с самой идеи ленивых вычислений. Идея простая -- у нас есть некие вычисления, которые мы хотим сделать не прямо сейчас, а когда-нибудь потом, когда понадобится. Почему мы можем захотеть так сделать, увидим ниже на примерах.

Язык Хаскель возводит эту идею в абсолют, в нем по умолчанию все вычисления ленивые. Но большинство языков предпочитают энергичные вычисления. (Энергичное -- противоположность ленивому вычислению, код выполняется сразу).

На базовом уровне это реализуется просто -- создаем функцию, сохраняем ее в переменную, вызываем позже. Сложность появляется, когда мы хотим комбинировать такие функции вместе, и выполнять всю комбинацию целиком. Это и делает модуль [Stream](https://hexdocs.pm/elixir/Stream.html).

Здесь мы увидим такой же набор функций, как в модуле Enum: map, filter, zip и др. Но давайте посмотрим, в чем отличие.

Возьмем некую коллекцию и прогоним ее через цепочку функций:

```elixir-eix
iex(3)> [1, 2, 3, 4, 5] |>
...(3)> Enum.map(&(&1 * &1)) |>
...(3)> Enum.zip([:a, :b, :c, :d, :e]) |>
...(3)> Enum.filter(fn({n, a}) -> n > 5 and a != :c end)
[{16, :d}, {25, :e}]
```

В цепочке 3 функции, и мы прошли по всему списку 3 раза. После каждой функции мы получаем промежуточные данные -- новый список и подаем его в следущую функцию. В сравнении с обычным циклом в императивном языке мы расходуем в 3 раз больше CPU и в 3 раза больше памяти.

Теперь сделаем тоже самое с модулем Stream:

```elixir-eix
iex(4)> [1, 2, 3, 4, 5] |>
...(4)> Stream.map(&(&1 * &1)) |>
...(4)> Stream.zip([:a, :b, :c, :d, :e]) |>
...(4)> Stream.filter(fn({n, a}) -> n > 5 and a != :c end) |>
...(4)> Enum.to_list
[{16, :d}, {25, :e}]
```

Результат получился тот же. Обратите внимание, что в конце цепочки мы добавили Enum.to_list. Этот вызов является энергичным вычислением, и именно он запускает всю цепочку. Без него результат будет таким:

```elixir-eix
iex(5)> [1, 2, 3, 4, 5] |>
...(5)> Stream.map(&(&1 * &1)) |>
...(5)> Stream.zip([:a, :b, :c, :d, :e]) |>
...(5)> Stream.filter(fn({n, a}) -> n > 5 and a != :c end)
#Stream<[
  enum: #Function<67.104660160/2 in Stream.zip/1>,
  funs: [#Function<39.104660160/1 in Stream.filter/2>]
]>
```

Это некая структура данных, хранящее ленивое вычисление, которое пока еще не запустилось.

```elixir-eix
iex(9)> lazy_computation = [1, 2, 3, 4, 5] |>
...(9)> Stream.map(&(&1 * &1)) |>
...(9)> Stream.zip([:a, :b, :c, :d, :e]) |>
...(9)> Stream.filter(fn({n, a}) -> n > 5 and a != :c end)
#Stream<[
  enum: #Function<67.104660160/2 in Stream.zip/1>,
  funs: [#Function<39.104660160/1 in Stream.filter/2>]
]>
iex(10)> Enum.to_list(lazy_computation)
[{16, :d}, {25, :e}]
```

Здесь мы составили композицию из ленивых функций, сохранили ее в переменную, а позже запустили вычисление через вызов энергичной функции.

Первая выгода, которую мы получили -- проход по списку выполняется только один раз. И здесь нет промежуточных результатов вычислений, на хранение которых нужно расходовать память. То есть это сравнимо по эффективности с циклом в императивном языке.

Но это не единственная выгода.


## Большие коллекции

Если мы создадим список из 10 миллионов элементов, то такой список займет много памяти и будет долго обрабатываться. Даже если нам нужны не все эти элементы, а лишь небольшая их часть:

```elixir-eix
iex(14)> Enum.map(1..10_000_000, &(&1 + 1)) |> Enum.take(5)
[2, 3, 4, 5, 6]
```

Нам нужны только первые 5 элементов, но вычисление заняло несколько секунд.

```elixir-eix
iex> Stream.map(1..10_000_000, &(&1+1)) |> Enum.take(5)
[2, 3, 4, 5, 6]
```

С модулем Stream такой же код выполняется мгновенно, потому что Stream не создает и не обрабатывает всю коллекцию, а только ту её часть, которая нужна для получения результата.

Более практичный пример -- чтение данных из внешнего мира: из файла или по сети через сокет. В случае с файлом данных может быть очень много, десятки гигабайт. В случае с сокетом размер данных вообще не известен, сокет может быть открыт неделями и месяцами, и все это время получать данные.

Возьмем пример с чтением из файла. Допустим, у нас есть некий словарь, хранящий термины и аббревиатуры в таком виде:

```shell
$ cat data/dictionary.txt
ISO 8601: Date and time format
MIT: Massachusetts Institute of Technology
OpenGL: Open Graphics Library
OSF: Open Software Foundation
SASL: Simple Authentication and Security Layer
TLS: Transport Layer Security
UUID: universally unique identifier
```

И, допустим, этот файл имеет размер 15 Гб.

Мы хотим найти в нем самый длинный термин:

```elixir-eix
def find_longest(file) do
  File.read!(file)
  |> String.split("\n")
  |> Enum.map(fn(line) -> String.split(line, ":") end)
  |> Enum.map(fn([term | _]) -> term end)
  |> Enum.map(fn(term) -> {String.length(term), term} end)
  |> Enum.max_by(fn({len, _term}) -> len end)
  |> elem(1)
end
```
В этой реализации мы загружаем в память весь файл, 15 Гб, и 4 раза выполняем проход по словарю.

Воспользуемся Stream:

```elixir-eix
def find_longest_lazy(file) do
  File.stream!(file)
  |> Stream.map(fn(line) -> String.split(line, ":") end)
  |> Stream.map(fn([term | _]) -> term end)
  |> Stream.map(fn(term) -> {String.length(term), term} end)
  |> Enum.max_by(fn({len, _term}) -> len end)
  |> elem(1)
end
```

Функция File.stream! возвращает ленивую коллекцию, отдающую данные из файла построчно. В этом случае мы загружаем данные в память небольшими порциями и выполняем только один проход по нему. Последний вызов в цепочке должен активировать вычисления, поэтому там Enum, а не Stream.

Этот код не обязательно будет быстрее, т.к. мы много раз читаем данные с диска, но он точно расходует меньше оперативной памяти.


## Бесконечные коллекции

Коллекции бывают не просто большие, они бывают бесконечные. В модуле Stream есть 5 функций, которые генерируют такие коллекции: **cycle**, **repeatedly**, **iterate**, **unfold** и **resource**. Рассмотрим некоторые из них.

### Stream.cycle

`Stream.cycle/1` принимает на вход коллекцию и генерирует бесконечную коллецию, состоящую из повторения элементов исходной:

```elixir-eix
iex(9)> Stream.cycle([1, 2, 3]) |> Enum.take(20)
[1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2]
```

Это можно использовать, например, для генерации html таблицы с чередующимся фоновым цветом для строк:

```elixir
data = ["row 1", "row 2", "row 3", "row 4", "row 5"]

def make_table(data) do
  content =
    Stream.cycle(["white", "gray"])
    |> Stream.zip(data)
    |> Enum.map(fn {bg, row} ->
      "<tr><td class='#{bg}'>#{row}</td></tr>"
    end)
    |> Enum.join("\n")

  "<table>" <> content <> "</table>"
end

<tr class='white'><td>row 1</td></tr>
<tr class='grey'><td>row 2</td></tr>
<tr class='white'><td>row 3</td></tr>
<tr class='grey'><td>row 4</td></tr>
<tr class='white'><td>row 5</td></tr>
```

### Stream.iterate

`Stream.iterate/2` принимает некое начальное значение и функцию, которая будет генирировать новое значение. На каждой итерации эта функция принимает свой предыдущий результат и на его основе генерирует новый результат:

```
iex(3)> iterator = Stream.iterate(1, fn arg -> arg * 2 end)
#Function<63.53678557/2 in Stream.unfold/2>
iex(4)> iterator |> Enum.take(10)
[1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
```

Сделаем таблицу посложнее -- к чередованию фона ещё добавим нумерацию рядов:

```
def make_table_2(data) do
  css_styles = Stream.cycle(["white_bg", "gray_bg"])
  iterator = Stream.iterate(1, fn a -> a + 1 end)

  content =
    Stream.zip(css_styles, iterator)
    |> Enum.zip(data)
    |> Enum.map(fn {{css_style, index}, row} ->
      "<tr class='#{css_style}'><td>#{index}</td><td>#{row}</td></tr>"
    end)
    |> Enum.join("\n")

  "<table>\n" <> content <> "\n</table>"
end
```

### Stream.unfold

`Stream.unfold/2` можно рассматривать как функцию, противоположную fold (reduce). Если fold сворачивает список в одиночное значение, то unfold разворачивает список из одиночного значения.

Она принимает на вход начальное состояние и разворачивающую функцию. Разворачивающая функция принимает на вход текущее состояние и возвращает кортеж из двух значений. Первый элемент кортежа -- это то, что становится очередным элементом списка. Второй элемент кортежа, это ново
е состояние, которое передается в разворачивающую функцию на следущем шаге.

То есть, это более сложная версия функции `iterate`.

```elixir
unfolder = fn state -> { curr_value, new_state } end
Stream.unfold(initial_state, unfolder)
```

Обратите внимание на симметричность относительно функции fold (reduce):

```elixir
folder = fn(curr_value, acc) -> new_acc end
Enum.reduce(collection, folder)
```

Пример, как генерируются значения:

```
def make_table_3() do
  initial_state = {true, 1}

  unfolder = fn {odd, index} ->
    value = %{odd: odd, index: index}
    new_state = {not odd, index + 1}
    {value, new_state}
  end

  Stream.unfold(initial_state, unfolder)
end

iex(4)> Lazy.make_table_2() |> Enum.take(10)
[
  %{index: 1, odd: true},
  %{index: 2, odd: false},
  %{index: 3, odd: true},
  %{index: 4, odd: false},
  %{index: 5, odd: true},
  %{index: 6, odd: false},
  %{index: 7, odd: true},
  %{index: 8, odd: false},
  %{index: 9, odd: true},
  %{index: 10, odd: false}
]
```

Применим опять для создания таблицы:

```
def test_data do
  [
    {"Bob", 24},
    {"Bill", 25},
    {"Kate", 26},
    {"Helen", 34},
    {"Yury", 16}
  ]
end

def make_table_3(users) do
  initial_state = {true, 1}

  unfolder = fn {odd, index} ->
    value = %{odd: odd, index: index}
    new_state = {not odd, index + 1}
    {value, new_state}
  end

  rows =
    Stream.unfold(initial_state, unfolder)
    |> Stream.zip(users)
    |> Enum.map(fn {state, user} ->
      css_style = if state.odd, do: "white", else: "gray"
      {name, age} = user
      "<tr class='#{css_style}'><td>#{state.index}</td><td>#{name}</td><td>#{age}</td></tr>"
    end)
    |> Enum.join("\n")

  "<table>#{rows}</table>"
end
```


Кстати, вы могли заметить, что функции reduce/fold в модуле Stream нет. В теории можно было бы сделать и ленивый reduce, но в Эликсир не стали это делать.
