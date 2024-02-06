# Числа с плавающей точкой (Float)

Числа с плавающей точкой реализованы по стандарту [IEEE 754](https://ru.wikipedia.org/wiki/IEEE_754-2008), как и в большинстве других языков. Это значит, что их точность ограничена, и при некоторых операциях возможна потеря точности:

```elixir-iex
iex(1)> 0.1 + 0.2
0.30000000000000004
```

В Эликсир нет разделения на Float и Double. Все Float числа занимают 64 бита памяти.

Для них также реализованы обычные арифметические операции:

```elixir-iex
iex(2)> 20 + 20.5
40.5
iex(3)> 50.0 - 14.5
35.5
iex(4)> 2 * 7.3
14.6
iex(5)> 12.15 / 1.5
8.1
```

Операторы `+ - *` возвращают целое число, если оба аргумента целые, и число с плавающей точкой, если хотя бы один из аргументов с плавающей точкой. Оператор `/` всегда возвращает число с плавающей точкой.

## Упражнение

Реализуем функцию `equal?/3`, которая сравнивает два float значения на равенство с допустимой погрешностью. Погрешность передается 3-м аргументом.

```elixir
defmodule FloatExample do
  def equal?(f1, f2, precision \\ 0.01) do
    abs(f1 - f2) < precision
  end
end
```

Компилируем и проверяем:

```elixir-iex
iex(1)> c "lib/float_example.exs"
[FloatExample, FloatExampleTest]
iex(2)> FloatExample.equal?(0.1, 0.2)
false
iex(3)> FloatExample.equal?(0.1, 0.11)
true
iex(4)> FloatExample.equal?(0.1, 0.11, 0.001)
false
```

Добавим тесты на разные случаи, какие сможем придумать:

```elixir
ExUnit.start()

defmodule FloatExampleTest do
  use ExUnit.Case
  import FloatExample

  test "equal?" do
    assert equal?(3.5, 3.5)
    assert equal?(3.51, 3.51)
    assert not equal?(3.51, 3.53)
  end

  test "equal? with precision" do
    assert equal?(3.5, 3.5, 0.01)
    assert equal?(3.51, 3.51, 0.01)
    assert not equal?(3.51, 3.53, 0.01)
    assert equal?(3.51, 3.53, 0.1)
    assert not equal?(3.501, 3.503, 0.001)
    assert equal?(3.501, 3.503, 0.01)
  end

  test "equal? with negative numbers" do
    assert equal?(-7.77, -7.75, 0.1)
    assert equal?(-10.95, -11.0, 0.2)
    assert equal?(-10.95, -11.0, 0.06)
    assert not equal?(-10.95, -11.0, 0.02)
  end
end
```

Запускаем тесты:

```shell
$ elixir lib/float_example.exs
...
Finished in 0.2 seconds (0.2s on load, 0.00s async, 0.03s sync)
3 tests, 0 failures

Randomized with seed 479648
```
