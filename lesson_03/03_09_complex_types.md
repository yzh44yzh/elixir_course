# Сложные типы (Complex types)

## IO List

Рекурсивный тип данных, который состоит из:
- byte (число в диапазоне 0-255)
- [byte] (список таких чисел)
- String.t (бинарная строка)
- IO List (включает сам себя)

TODO: пример

```
> ... |> flatten()
"Hello world!"
```

Такой тип поддерживается во всех операциях ввода-вывода и позволяет избежать дорогостоящей конкатенации строк.

Не эффективно:

```elixir-iex
iex(8)> header = "<html><body>"
"<html><body>"
iex(9)> footer = "</body></html>"
"</body></html>"
iex(10)> name = "Bob"
"Bob"
iex(11)> body = "hello " <> name <> "!"
"hello Bob!"
iex(12)> page = header <> body <> footer
iex(15)> IO.puts page
<html><body>hello Bob!</body></html>
```
TODO:
page = title <> body
html = header <> page <> footer

Здесь много операций по копированию строк из одной области памяти в другую, чтобы в итоге результат разместился в одной непрерывной области памяти.

Эффективно:

```elixir-iex
iex(8)> header = "<html><body>"
"<html><body>"
iex(9)> footer = "</body></html>"
"</body></html>"
iex(10)> name = "Bob"
"Bob"
iex(13)> body_io = ["hello ", name, "!"]
["hello ", "Bob", "!"]
iex(14)> page_io = [header, body_io, footer]
["<html><body>", ["hello ", "Bob", "!"], "</body></html>"]
iex(16)> IO.puts page_io
<html><body>hello Bob!</body></html>
:ok
```
TODO:
page = [title, body]
html = [header, page, footer]

Здесь нет копирования, а результат представляет собой дерево из ссылок на разные области памяти.

Применяется везде, где есть IO: запись в файл, запись в сокет.

TODO: например.
запись в файл -- записать IO list, открыть файл, посмотреть


## Keyword List

Легаси со времен, когда BEAM не поддерживала map.

Список кортежей из двух элементов, где первых элемент -- atom() -- ключ, а второй элемент -- значение.

Активно используются до сих пор, хотя вроде бы незачем.

TODO примеры

[{:a, 42}, {:b, 50}]
[a: 42, b: 50]

в отличие от map сохраняют порядок ключей
и могут иметь несколько значений под одним ключом

Сложившаяся традиция:
options -- настройки
Много функций в стандартной библиотеке принимают список options в виде keyword list.

TODO: пример.
String.split мы использовали

TODO: ещё пример
Например функция `IO.inspect/2`, которую можно использовать для отладочной печати.
Суть её работы в том, чтобы принять значение и распечатать в консоль.
Функция позволяет управлять своим поведением через "опции" которые передаются
через последний аргумент, являющийся keyword list`ом:

```elixir-iex
iex(1)> h IO.inspect
# => @spec inspect(item, keyword()) :: ...
iex(2)> IO.inspect([100, 200, 300])
[100, 200, 300]
iex(3)> IO.inspect([100, 200, 300], [width: 3])
[100,
 200,
 300]
```
Такой подход стал настолько широко использоваться что специально для удобства
ввели синтаксический сахар в виде сокращенного синтаксиса, позволяя явно не
указывать скобки для keyword list-а:
```elixir-iex
iex(4)> IO.inspect([100, 200, 300], width: 3, limit: 1)
[100,
 ...]
```
Визуально кажется, что здесь используется varargs(передача переменного количества
аргументов) Но на самом деле Эликсир не поддерживает такой вид передачи и на
деле просто ключи оборачиваются в keyword-список.
Т.е. такой вид полностью аналогичен этому:
```elixir-iex
iex(5)> IO.inspect([100, 200, 300], [width: 3, limit: 1])
[100,
 ...]
```

Конфигурационные файлы тоже по сложившейся традиции.
TODO: пример коннекта к БД


## Range

Range - это абстракция позволяющая представляять диапазоны чисел.
```elixir
iex> range = 1..2         # синтаксис обьявления диапазона
iex> 2 in range           # => true
iex> -1 in range          # => false
```

`in` - оператор проверки находится ли заданное число в диапазоне.

Range реализует протокол Enumerable, а значит все функции из модуля Enum могут
с ним работать.

(Note: В Эликсир Протокол нечто похожее на интерфейс в Java(и др. ООП языках).
Упрощенно протокол можно воспринимать как своего рода обещание некой "сущности"
уметь выполнять некое действие принимая на вход заранее обговоренные данные.
Подробнее по протоколам будет в 7 уроке)

Пример функция `Enum.each/2` - позволяет вызвать для каждого
элемента коллекции(перчисления) переданную через аргумент анонимную функцию(лямбду)

```elixir
iex> Enum.each(1..3, fn val -> IO.puts(val) end)      # или кратко &IO.puts/1
1
2
3
```

Под копотом Range - это не отдельный тип данных, а надстройка над Map(Словарём),
содержащий в себе только заданные границы, а не все входящие в эти границы значения.
Поэтому Range экономен к памяти и занимает мало места, причем не зависимо от того
насколько большой диапазон он описывает из 10 цифр или из несколько миллионов.
На деле диапазон(Range) в памяти занимает столько же место сколько небольшой
словарь(Map). Почему это так увидем чуть ниже.

Диапазоны(Ranges) могут быть как восходящими(ascenging) так и
нисходящими(descending). Зависит от заданного шага который по умолчанию = 1
Причем указанные граничные значения всегда входят в сам диапазон.

```elixir
iex> 1..8        # диапазон от 1 до 8 включая 1 и 8
iex> ?A..?H      # диапазон от 65 до 72 (коды ASCII символов от A до H)
```

Строим список целых чисел из диапазона
```elixir
iex> Enum.to_list(1..8)
[1,2,3,4,5,6,7,8]

# немного магии и числа превращаются в буквы-строки (Вспоминаем BitString)
iex> Enum.map(?A..?H, fn codepoint -> <<codepoint>> end)    # или &(<<&1>>)
["A", "B", "C", "D", "E", "F", "G", "H"]
```

Если интересно, то можно запросить подробный отчёт о том, что из себя
представляет конкретное значени. Здесь например мы спрашиваем о значении Range:
```elixir
iex> i 1..4
Term
  1..4
Data type
  Range
Description
  This is a struct representing a range of numbers. It is commonly
  defined using the `first..last//step` syntax. The step is not
  required and defaults to 1.
Raw representation
  %Range{first: 1, last: 4, step: 1}                     # <<< !
Reference modules
  Range
Implemented protocols
  Enumerable, IEx.Info, Inspect
```
О структурах(Struct) будет чуть позже, но здесь можно заметить что сырое
представление диапазона(Range) очень похоже на словарь(Map) которую мы уже
изучили. И как видим содержит он в себе 3 ключа: начало, конец и шаг:
`%Range{first: 1, last: 4, step: 1}`


Немного выйдем за рамки привычного и проявим больше любопытства.
"А Что такое `i` в iex-консоли?"
(Привыкаем самостоятельно находить ответы на вопросы и работать со встроенной
документацией)
```
iex>h i
                              def i(term \\ v(-1))

Prints information about the data type of any given term.

If no argument is given, the value of the previous expression is used.

## Examples

    iex> i(1..5)

Will print:

    Term
      1..5
    Data type
      Range
    Description
      This is a struct. Structs are maps with a __struct__ key.
    Reference modules
      Range, Map

```

1..10 |> Enum.to_list()
a in range
iterate over range

Build on top of map.


## Sigil

~~~
Note sigil - на русский можно перевести как символ-знак-миниатюра.
Смысл этого слова - некий уникальный знак описывающий некую сущность.
Например в астрологии этим словом обозначают планету или созвездие.
Здесь же для лучшего понимания использую оригинальный термин без перевода.
~~~

Эликсир задуман как расширяемый язык. Идея в том, чтобы дать разработчикам
возможность расширять язык под нужды их конкретной предметной области.
Sigil-ы как раз и обеспечивают фундамент для расширения языка Эликсир,
позволяя создавать свои собственные представления данных в виде текста.
Sigil-ы начинаются с символа тильда(`~`) затем указывается либо прописной символ
(lower-case) либо заглавный(upper-case) и следом за ним разделитель.
в определении sigil-а разделителя должно быть два. Перед символом и в конце
текстового представления данных. После последнего разделителя можно
указывать необязательные(опциональные) модификаторы для данного Sigil-а.

В языке есть уже встроенные Sigil-ы, позволяющие удобно и быстро представлять
некие структуры данных через их текстового представление.

Один из таких Sigil-ов(`~w` мы уже использовали в тесте для выравнивания строк):
```elixir
assert ["  cat   ", " zebra  ", "elephant"] == align(~w'cat zebra elephant')
```

### Word lists Список слов.

Sigil `~w` используется для построения списка слов. Слова здесь - это просто
обычные бинарные строки(BitString) Внтури `~w`-сигила слова разделяются пробелами

```elixir
iex> ~w'one two three'
["one", "two", "three"]
```

Кроме того `~w`-сигилу можно указать тип элементов которые мы хотим построить из
текстового представления. Делается это через указание модификаторов:
`c`, `s`, `a` соответственно для charlist-ов, строк и атомов.

```elixir
iex> ~w'one two three'a
[:one, :two, :three]

iex> ~w'one two three'c
[~c"one", ~c"two", ~c"three"]
```


### Регулярные выражения (Regular expressions)

Наиболее часто используемый сигил в Эликсир это `~r`.
Через него можно создать регулярные выражения:

```elixir
# Регулярка для сопоставления(matching) строк содержащих "elixir" или "erlang"
iex> regex = ~r/elixir|erlang/
~r/elixir|erlang/
iex> "elixir" =~ regex
true
iex> "erlang" =~ regex
true
iex> "another_one" =~ regex
false
```

Эликсир предоставляет Perl-совместимые регулярные выражения(regexes), которые
реализуютсяя через библиотеку [PCRE](http://www.pcre.org/)
Регулярки так же поддерживают модификаторы. Например модификатор `i` делает
регулярку не чувствительной к регистру:

```elixir
iex> "ELIXIR" =~ ~r/elixir/
false
iex> "ELIXIR" =~ ~r/elixir/i
true
```

Посмотреть все возможные модификаторы можно через справку для модуля `Regex`.
```elixir-iex
iex> h Regex
                                     Regex

Provides regular expressions for Elixir.

Regex is based on PCRE (Perl Compatible Regular Expressions) and built on top ..
```

Создатели Эликсир достаточно неплохо продумали дизайн sigil-ов.
Для того чтобы сделать код более читаемым и простым, и чтобы избегать
экранирования некоторых символов можно использовать разные разделители в сегиле.
В примерах выше для regex мы использовали разделитель в виде слэша `/` в
примере со списком слов(`~w`) разделителем у нас выступала одинарная ковычка `'`.
Вообще же разделителей в сигилах существует `8` разных видов:
Для наглядности пример для регулярки(но это будет работать и со всеми другими
сигилами тоже):

```elixir
~r/word/
~r|word|
~r"word"
~r'word'
~r(word)
~r[word]
~r{word}
~r<word>
```

Основная идея в таком разнообразии разделителей в сигилах - это предоставить
возможность описывать литералы избегая использования символов экранирования.
Без этой возможности пришлось бы часть кода экранировать, а это делает код более
тяжелым для восприятия и увеличивает вероятность ошибок.

Вот явный тому пример:
```elixir
# регулярка перегружена символами экранирования
iex> "http://domain.org" =~ ~r/^https?:\/\//
true

iex> "http://domain.org" =~ ~r(^https?://)
true
```
Если же например регулярное выражение содержит слэш и группы, которые
описываются через круглые скобки(`()`) то можно выбрать любой другой свободный
разделитель из 8 доступных, чтобы избежать экранирования.


### Строки (Strings)

Сигил `~s` используется для создания строк. Тех самых обычных бинарных строк,
которые обычно мы создаём через двойные кавычки.
`~s`- может быть полезен когда строка должна содержать кавычки.
```elixir
iex> ~s(this is a string with "double" quotes, not 'single' ones)
"this is a string with \"double\" quotes, not 'single' ones"
```

### Списки символов (Charlists)

Как мы помним в Эликсир есть два вида строк - бинарные строки(BitString) и
строки представляемые через список целых чисел(integer).
В старых версиях Эликсир(Хотя это по-прежнему работает и на последней современной)
charlist создавался через одинарные кавычки. Ныне же создаётся через `~c`-сигил

```elixir
iex> ~c"this-is-charlist"
~c"this-is-charlist"

# Запрашиваем подробный отчёт по значению из предыдущей строки
iex> i
Term
  ~c"this-is-charlist"
Data type
  List
Description
  This is a list of integers that is printed using the `~c` sigil syntax,
  defined by the `Kernel.sigil_c/2` macro, because all the integers in it
  represent printable ASCII characters. Conventionally, a list of Unicode
  code points is known as a charlist and a list of ASCII characters is a
  subset of it.
Raw representation
  [116, 104, 105, 115, 45, 105, 115, 45, 99, 104, 97, 114, 108, 105, 115, 116]
```

```elixir
iex> [?w, ?o, ?r, ?d]
~c"word"

iex> ~c(this is a charlist with "double" quotes, not 'single' ones)
~c"this is a charlist with \"double\" quotes, not 'single' ones"
```


### Интерполяция и экранирование в строковых sigil-ах

Эликсир поддерживает разные варианты одного и тогоже сигила,
для особой обработки символов экранирования и интерполяции данных в строках.
Так обычно заглавные(uppercase) сигил-символы не выполняют ни интерполяцию ни
экранирование и выдают строку такой какая она есть.
Например `~s` и `~S` оба создают бинарную-строку, но `~s` будет выполнять
интерполяцию и экранирование, а `~S` будет просто выдавать строку как есть:

```elixir
iex> ~s(String with escape codes \x26 #{"inter" <> "polation"})
"String with escape codes & interpolation"

iex> ~S(String without escape codes \x26 without #{interpolation})
"String without escape codes \\x26 without \#{interpolation}"
```

Так же стоит отметить что heredocs - блоки, которые задаются через пару трёх
двойных кавычек так же могут быть заданы через строковый сигил `~s`
```elixir
iex> ~s"""
   > this is
   > a heredoc string
   > """
```

Чаще всего это используется в документации.
Опять таки чтобы не возиться с экранированием.
```elixir
@doc ~S"""
Converts double-quotes to single-quotes.

## Examples

    iex> convert("\"foo\"")
    "'foo'"

"""
def convert(...)
```

Если не использовать здесь строковый сигил(`~S`) пришлось бы экранировать:
`iex> convert("\\\"foo\\\"")`


### Сигилы для даты и времени

В эликсире "сразу из коробки" идут несколько сигилов для работы с разного типа
данными представляющими время и дату.
Время и дата хранятся в структурах(struct), о том что такое структуры будет чуть
позже. Пока же упрощенно можно их воспринимать как обёртку над словарём(Map),
где ключи-атомы представляют собой поля структуры.

#### Date
Структура для хранения даты. Для её текстового описания используем sigil `~D`:

```elixir
iex> d = ~D[2024-03-06]
~D[2024-03-06]

iex> {d.year, d.month, d.day, d.calendar}
{2024, 3, 6, Calendar.ISO}

iex> i d
Term
  ~D[2024-03-06]
Data type
  Date
Description
  This is a struct representing a date. It is commonly
  represented using the `~D` sigil syntax, that is
  defined in the `Kernel.sigil_D/2` macro.
Raw representation
  %Date{year: 2024, month: 3, day: 6, calendar: Calendar.ISO}
Reference modules
  Date, Calendar, Map
Implemented protocols
  IEx.Info, Inspect, String.Chars
```

Как видим из вывода функции `i` поля, которые содержит структура легко можно
узнать из секции `Raw representation`. Здесь это year, month, day и calendar.
Обращаться к этим полям можно как в обычном словаре(Map) к ключам: `d.day`


#### Time
Для создания времени используем сигил `~T`
```elixir
iex> t = ~T[09:50:10]
~T[09:50:10]
iex> i t
  ...
Raw representation
  %Time{hour: 9, minute: 50, second: 10, microsecond: {0, 0}, calendar: Calendar.ISO}
```

#### NaiveDateTime

Эта структура содержит в себе поля из `Date` и `Time`. Создаётся через `~N`:
```elixir
iex> ndt = ~N[2024-03-06 09:56:32]
~N[2024-03-06 09:56:32]
```
Наивной эта стуктура называется т.к. не содержит часового пояса (timezone).


#### UTC DateTime

Более полноценная структура содержащее и время и дату и часовой пояс.
Содержит в себе все те же поля что в NaiveDateTime плюс поля для хранения
временной зоны. Создать эту структуру можно через сигил `~U`:

```elixir
iex> dt = ~U[2024-03-06 09:56:32Z]
~U[2024-03-06 09:56:32Z]

iex> %DateTime{minute: min, time_zone: tz} = dt
~U[2024-03-06 09:56:32Z]
iex> min
56
iex> tz
"Etc/UTC"
iex> d.year
2024
iex> d.month
3
iex> Map.from_struct(dt)
%{
  microsecond: {0, 0},
  second: 32,
  calendar: Calendar.ISO,
  month: 3,
  day: 6,
  year: 2024,
  minute: 56,
  hour: 9,
  time_zone: "Etc/UTC",
  zone_abbr: "UTC",
  utc_offset: 0,
  std_offset: 0
}
iex> i dt
Term
  ~U[2024-03-06 09:56:32Z]
Data type
  DateTime
Description
  This is a struct representing a datetime. It is commonly
  represented using the `~U` sigil syntax, that is
  defined in the `Kernel.sigil_U/2` macro.
Raw representation
  %DateTime{year: 2024, month: 3, day: 6, hour: 9, minute: 56, second: 32, time_zone: "Etc/UTC", zone_abbr: "UTC", utc_offset: 0, std_offset: 0, microsecond: {0, 0}, calendar: Calendar.ISO}
Reference modules
  DateTime, Calendar, Map
Implemented protocols
  IEx.Info, Inspect, String.Chars
```


### Custom sigils

Как уже было сказано выше sigil-ы в Эликсир расширяемые.
Фактически sigil `~r/foo/i` эквивалентен вызову `sigil_r` с двумя аргументами
бинарной строкой и списком:

```elixir
iex> sigil_r(<<"foo">>, [?i])
~r/foo/i

# вот так можно посмотреть документацию для сигила ~r через функцию sigil_r
iex> h sigil_r
...
```

Можно создать свой собственный sigil, реализовав функцию с имя которой будет
соответствовать шаблону `sigil_{character}`.

Для примера реализуем совой `~i`-sigil, который из текстового представления
будет создавать целое число (initeger).
Через необязательный модификатор сделаем возможным делать число отрицательным.


```elixir-iex
iex(1)> defmodule MySigils do
...(1)>   def sigil_i(string, []), do: String.to_integer(string)
...(1)>   def sigil_i(string, [?n]), do: -String.to_integer(string)
...(1)> end

iex(2)> import MySigils
iex(3)> ~i(42)
42
iex(4)> ~i(42)n
-42
```

Свои кастомные sigil-ы могут состоять либо из одного прописного символа либо
из нескольких заглавных символов.

Какие еще есть случаи использования сигилов и особенности их работы?

Sigil-ы cовместно с макросами можно использовать для выполнения некой
полезной работы которая будет происходить во время компиляции. Например,
регулярки в Эликсир компилируются в эффиктивное представление во время
компиляции исходного эликсир-кода в байткод. А это значит что компилировать
регулярные выражения во время выполнения программы(runtime) будет уже не нужно.
(А это экономия ресурсов системы).

Если данная тема заинтересовала смотри про макросы и в исходники самого языка.
(то как sigil-ы реализованы в модуле Kernel.(функции начинающиеся с `sigil_*`))

