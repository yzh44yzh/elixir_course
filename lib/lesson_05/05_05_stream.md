# Stream

Delay the Function Call
Sometimes you want to give developers the flexibility to decide when a function
will be evaluated, by building a new function using the existing one. 

Other func-
tional languages have currying, which is a feature that delays a function’s evalu-
ation when you pass fewer arguments than the function requires. Elixir has partial
application, a feature you can use to postpone a function’s execution by wrapping
it in a new function and fixing a value to any of the function’s arguments.

```
iex> Enum.map(1..10_000_000, &(&1+1)) |> Enum.take(5)
[2, 3, 4, 5, 6]
```
it takes about 8 seconds before I see the result. Elixir is creating a 10-million-
element list, then taking the first five elements from it. If instead I write
```
iex> Stream.map(1..10_000_000, &(&1+1)) |> Enum.take(5)
[2, 3, 4, 5, 6]
```
the result comes back instantaneously. The take call just needs five values,
which it gets from the stream. Once it has them, there’s no more processing.


TODO пример Enum.map |> Enum.map |> Enum.filter -- несколько проходов по списку.
То же самое со Stream -- один проход.

```
[1,2,3,4]
|> Stream.map(&(&1*&1))
|> Stream.map(&(&1+1))
|> Stream.filter(fn x -> rem(x,2) == 1 end)
|> Enum.to_list
```

to make iteration happen последний в pipeline должен быть Enum
to_list, take

consuming slow and potentially large input
особенно из внешнего мира -- файл или сокет
File.stream! позволяет не загружать сразу весь файл в память, а читать его построчно.

```
IO.puts File.read!("/usr/share/dict/words")
|> String.split
|> Enum.max_by(&String.length/1)
```
In this case, we read the whole dictionary into memory (on my machine that’s
2.4MB), then split it into a list of words (236,000 of them) before processing
it to find the longest (which happens to be formaldehydesulphoxylate).

```
IO.puts File.open!("/usr/share/dict/words")
|> IO.stream(:line)
|> Enum.max_by(&String.length/1)
```
The magic here is the call to IO.stream , which converts an IO device (in this
case the open file) into a stream that serves one line at a time. In fact, this is
such a useful concept that there’s a shortcut:
```
IO.puts File.stream!("/usr/share/dict/words") |> Enum.max_by(&String.length/1)
```
The good news is that there is no intermediate storage. 
The bad news is that it runs about two times slower than the previous version.

TODO Упражнения:
- длина каждой строки в файле
- самая длинная строка в файле
- количество слов в каждой строке файла, и во всем файле суммарно. 


## Устройство

Stream основаны на функциях, которые возвращают функции.
TODO какой-нибудь упрощенный пример, как устроены Stream.


## Infinite Streams

cycle, repeatedly, iterate, unfold, and resource.

Stream.cycle takes an enumerable and returns an infinite stream containing
that enumerable’s elements. When it gets to the end, it repeats from the
beginning, indefinitely. Here’s an example that generates the rows in an HTML
table with alternating green and white classes:
```
iex> Stream.cycle(~w{ green white }) |>
...> Stream.zip(1..5) |>
...> Enum.map(fn {class, value} ->
...>
"<tr class='#{class}'><td>#{value}</td></tr>\n" end) |>
...> IO.puts
<tr class="green"><td>1</td></tr>
<tr class="white"><td>2</td></tr>
<tr class="green"><td>3</td></tr>
<tr class="white"><td>4</td></tr>
<tr class="green"><td>5</td></tr>
:ok
```


Stream.unfold
звучит как противоположность fold (reduce) -- берем одиночное зрачение, и разворачиваем его в список.

You supply an initial value and
a function. The function uses the argument to create two values, returned as
a tuple. The first is the value to be returned by this iteration of the stream,
and the second is the value to be passed to the function on the next iteration
of the stream. If the function returns nil , the stream terminates.

it is a general way of creating
a potentially infinite stream of values where each value is some function of
the previous state.

The key is the generating function. Its general form is
fn state -> { stream_value, new_state } end

For example, here’s a stream of Fibonacci numbers:
iex> Stream.unfold({0,1}, fn {f1,f2} -> {f1, {f2, f1+f2}} end) |> Enum.take(15)
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]


TODO
Stream.resource -- можно взять как домашнее задание.
