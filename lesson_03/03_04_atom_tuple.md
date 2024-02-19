# Атомы и кортежи (Atom & Tuple)

**Атомы** типичны для функциональных языков, но редко встречаются в языках императивных.

Программисты на Ruby знакомы с Symbol, это тоже самое, что атомы. Но, например, в Python или JavaScript такого типа данных нет.

Атомы, это некие константные значения, которые можно сравнивать друг с другом. Собственно, сравнивать — это единственное, что с ними можно делать. Сами по себе они не очень полезны, но становятся полезны в комплекте с кортежами и сопоставлением с образцом (pattern matching).

Вместо атомов можно было бы использовать обычные строки. Но атомы занимают мало места в памяти, всего 4 байта, и поэтому эффективнее строк. Их можно рассматривать как человекочитаемые синонимы для числовых констант.

Атом начинается либо с большой буквы:

```elixir
User
Point
IP_Address
```

либо с двоеточия:

```elixir
:user
:point
:ip_address
```

**Кортеж (tuple)** — это структура данных, объединяющая несколько разных значений. Кортеж похож на список, но в отличие от списка имеет фиксированную длину.

Примеры:

```elixir
{"Bob", :male, 23}
{1, 2}
{127, 0, 0, 1}
```

Часто в кортежах на первой позиции ставят атом, чтобы обозначить, какие данные собраны в кортеже. Таким образом кортеж помечается тэгом, и это называется **тэгированный кортеж (tagged tuple)**:

```elixir
{:user, "Bob", :male, 23}
{:point, 1, 2}
{:ip_address, 127, 0, 0, 1}
```

Тегированные кортежи не так часто используются в Эликсир, но очень часто в Эрланг и других функциональных языках.

Кортежи могут быть вложенными:

```elixir
{:rectangle, {:point, 0, 0}, {:point, 10, 10}}
{:ip4, {127, 0, 0, 1}}
```

Небольшие объекты, состоящие из 2-х - 4-х полей, удобно представлять в виде кортежей, если роль полей понятна из контекста. Если полей больше 4-х, то лучше использовать **словарь (map)** или **структуру (struct)**.

Атомы и кортежи — это легковесные объекты, они используют меньше памяти, чем словари и структуры, и операции над ними выполняются быстрее.

Для работы с ними часто используется **сопоставление с образцом (pattern matching)**. По этой теме у нас будет отдельный урок. Сейчас важно знать, что это способ извлечь отдельные значения из кортежа.

Создаём кортеж:

```elixir-iex
iex(1)> my_point = {:point, 5, 10}
{:point, 5, 10}
```

Извлекаем значения из кортежа:

```elixir-iex
iex(2)> {:point, x, y} = my_point
{:point, 5, 10}
iex(3)> x
5
iex(4)> y
10
```

Извлечь значения можно с помощью функции `elem/2`:

```elixir-iex
iex(5)> elem(my_point, 1)
5
iex(6)> elem(my_point, 2)
10
```

Но на практике эту функцию используют крайне редко, потому что сопоставление с образцом удобнее.

## Упражнение №1

Реализуем функцию `distance/2`, которая вычисляет расстояние между двумя точками:

```elixir
defmodule AtomTupleExample do
  def distance(point1, point2) do
    {:point, x1, y1} = point1
    {:point, x2, y2} = point2
    x_dist = abs(x1 - x2)
    y_dist = abs(y1 - y2)
    :math.sqrt(:math.pow(x_dist, 2) + :math.pow(y_dist, 2))
  end
end
```

Здесь мы извлекам координаты каждой точки, вычисляем абсолютное расстояние по горизонтали и по вертикали, и затем вычиляем расстояние по диагонали с помощью теоремы Пифагора.

Функция `abs/1` возвращает модуль числа. Мы применяем её, чтобы получить абсолютное значение для расстояний по горизонтали и по вертикали. Это необязательно делать, так как мы потом возводим эти значения в квадрат. Но поскольку мы применяем теорему Пифагора, то полезно представить себе треугольник. А треугольник со сторонами отрицательной длины представить трудно :)

Для извлечения корня и возведения в квадрат применяется модуль **:math** из стандартной библиотеки языка Эрланг. Эликсир переопределяет большинство модулей Эрланг, но модуль **:math** не переопределяет. Любые библиотеки и модули Эрланг можно использовать в Эликсир.

В Эрланг принято называть модули с маленькой буквы -- `:math`, в Эликсир принято называть модули с большой буквы -- `Math`. В обоих случаях имена модулей являются атомами.

Значения из кортежей часто извлекают прямо в аргументах функции:

```elixir
defmodule AtomTupleExample do
  def distance({:point, x1, y1}, {:point, x2, y2}) do
    x_dist = abs(x1 - x2)
    y_dist = abs(y1 - y2)
    :math.sqrt(:math.pow(x_dist, 2) + :math.pow(y_dist, 2))
  end
end
```

Запускаем:

```elixir-iex
iex(1)> c "lib/atom_tuple_example.exs"
[AtomTupleExample, AtomTupleExampleTest]
iex(2)> AtomTupleExample.distance({:point, 0, 0}, {:point, 0, 5})
5.0
iex(3)> AtomTupleExample.distance({:point, 10, 10}, {:point, 5, 5})
7.0710678118654755
iex(4)> AtomTupleExample.distance({:point, 0, 4}, {:point, 3, 0})
5.0
```

Добавим тесты:

```elixir
defmodule AtomTupleExampleTest do
  use ExUnit.Case
  import AtomTupleExample

  test "distance" do
    assert 5.0 == distance({:point, 0, 0}, {:point, 0, 5})
    assert 5.0 == distance({:point, 5, 0}, {:point, 0, 0})
    assert 0.0 == distance({:point, 5, 5}, {:point, 5, 5})
    assert 5.0 == distance({:point, 0, 0}, {:point, 3, 4})
    assert 5.0 == distance({:point, 0, 0}, {:point, -3, -4})
  end
end
```

и проверим:

```elixir-iex
$ elixir lib/atom_tuple_example.exs
.....
Finished in 0.1 seconds (0.1s on load, 0.00s async, 0.00s sync)
1 test, 0 failures

Randomized with seed 19174
```

В тестах мы допустили некоторую небрежность -- мы сравниваем числа с плавающей точкой с помощью оператора сравнения `==`. Раньше мы говорили, что так делать нельзя, а нужно сравнивать их с учётом некоторой погрешности. Однако, в стандартной библиотеке нет средств для этого, и нужно писать свою функцию.

Такого рода небрежности бывают в реальных проектах. Но если вы (и я) их допускаете, то хотя бы делайте это сознательно, с пониманием рисков :) Так что когда это выстрелит на проде, вы будет знать, почему, и как это исправить.


## Упражнение №2

Сделаем ещё одно упражнение, реализуем функцию `point_inside_cirle?/2`, которая проверяет, что точка расположена внутри окружности.

Определим, что точка представлена кортежем `{:point, x, y}`. Окружность представлена кортежем `{:circle, center, radius}`, где `center` — это кортеж `:point`, а `radius` -- положительное целое число.

Определить это не сложно, нужно проверить, что расстояние от точки до центра окружности меньше, чем радиус. При этом мы можем использовать уже существующую функцию `distance/2`:

```elixir
  def point_inside_cicle?(point, {:circle, center, radius}) do
    distance(point, center) <= radius
  end
```

Проверяем:

```elixir-iex
iex(3)> r AtomTupleExample
iex(4)> AtomTupleExample.point_inside_circle?({:point, 3, 3}, {:circle, {:point, 0, 0}, 10})
true
iex(5)> AtomTupleExample.point_inside_circle?({:point, 13, 3}, {:circle, {:point, 0, 0}, 10})
false
```

Теперь сделаем аналогичную функцию `point_inside_rect?/2` для прямоугольника. Прямоугольник представлен кортежем `{:rect, left_top, right_bottom}`, где `left_top` и `right_bottom` — это кортежи `:point`.

```elixir
  def point_inside_rect?({:point, x, y}, {:rect, left_top, right_bottom}) do
    {:point, left_x, top_y} = left_top
    {:point, right_x, bottom_y} = right_bottom
    x >= left_x and x <= right_x and y <= top_y and y >= bottom_y
  end
```

Здесь несколько раз применили сопоставление с образцом, чтобы извлечь сперва точки прямоугольника, а потом координаты этих точек.

Проверяем:

```elixir-iex
iex(6)> r AtomTupleExample
iex(7)> AtomTupleExample.point_inside_rect?({:point, 5, 5}, {:rect, {:point, 0, 10}, {:point, 10, 0}})
true
iex(8)> AtomTupleExample.point_inside_rect?({:point, 5, 15}, {:rect, {:point, 0, 10}, {:point, 10, 0}})
false
```

Следущий шаг -- обобщим проверку для круга и для прямоугольника. На самом деле нам не нужны две разные функции для этого, мы может реализовать одну `point_inside_figure?/2`.

Мы опять немного забегаем вперед, в тему сопоставления с образцом. В Эликсир одна функция может иметь несколько реализаций (они называются "тело функции", по-английски "clause"). При вызове срабатывает только одно "тело функции", которое совпадёт по шаблону с переданными агрументами.

```elixir
  def point_inside_figure?(point, {:circle, center, radius}) do
    distance(point, center) <= radius
  end

  def point_inside_figure?({:point, x, y}, {:rect, left_top, right_bottom}) do
    {:point, left_x, top_y} = left_top
    {:point, right_x, bottom_y} = right_bottom
    x >= left_x and x <= right_x and y <= top_y and y >= bottom_y
  end
```

Здесь функция `point_inside_figure?`, имеет два тела. Первое сработает, если вторым аргументом передать круг. Второе тело сработает, если вторым аргументом передать прямоугольник:

```elixir-iex
iex(5)> AtomTupleExample.point_inside_figure?({:point, 3, 3}, {:circle, {:point, 0, 0}, 20})
true
iex(6)> AtomTupleExample.point_inside_figure?({:point, 3, 3}, {:rect, {:point, 0, 10}, {:point, 10, 0}})
true
```

Именно для этого сопоставление с образцом пишут прямо в агрументах функции. Оно служит шаблоном, который определяет, какое тело функции будет вызвано.

Если передать агрумент, который не сопадёт ни с одним шаблоном, то возникнет исключение:

```elixir-eix
iex(7)> AtomTupleExample.point_inside_figure?({:point, 3, 3}, {:triangle, {:point, 0, 0}, {:point, 5, 5}, {:point, 0, 5}})
** (FunctionClauseError) no function clause matching in AtomTupleExample.point_inside_figure?/2
```

Полная реализация выглядит так:

```elixir
defmodule AtomTupleExample do
  def distance({:point, x1, y1}, {:point, x2, y2}) do
    x_dist = abs(x1 - x2)
    y_dist = abs(y1 - y2)
    :math.sqrt(:math.pow(x_dist, 2) + :math.pow(y_dist, 2))
  end

  def point_inside_figure?(point, {:circle, center, radius}) do
    distance(point, center) <= radius
  end

  def point_inside_figure?({:point, x, y}, {:rect, left_top, right_bottom}) do
    {:point, left_x, top_y} = left_top
    {:point, right_x, bottom_y} = right_bottom
    x >= left_x and x <= right_x and y <= top_y and y >= bottom_y
  end
end
```

Добавим тесты:

```elixir
ExUnit.start()

defmodule AtomTupleExampleTest do
  use ExUnit.Case
  import AtomTupleExample

  test "distance" do
    assert 5.0 == distance({:point, 0, 0}, {:point, 0, 5})
    assert 5.0 == distance({:point, 5, 0}, {:point, 0, 0})
    assert 0.0 == distance({:point, 5, 5}, {:point, 5, 5})
    assert 5.0 == distance({:point, 0, 0}, {:point, 3, 4})
    assert 5.0 == distance({:point, 0, 0}, {:point, -3, -4})
  end

  test "point inside circle" do
    point = {:point, 50, 50}
    assert point_inside_figure?(point, {:circle, {:point, 10, 10}, 100})
    assert not point_inside_figure?(point, {:circle, {:point, -10, -10}, 20})
  end

  test "point inside rect" do
    point = {:point, -10, 20}
    assert point_inside_figure?(point, {:rect, {:point, -20, 30}, {:point, 20, 10}})
    assert not point_inside_figure?(point, {:rect, {:point, 0, 0}, {:point, 10, 10}})
  end
end
```

И запустим их:

```shell
$ elixir lib/atom_tuple_example.exs
.....
Finished in 0.2 seconds (0.1s on load, 0.00s async, 0.04s sync)
3 tests, 0 failures

Randomized with seed 709925
```

Полезно так же иметь негативные тесты. Если функция возвращает ошибку или бросает исключение, то негативный тест проверяет, что это нужная ошибка и нужное исключение.
/
В нашем случае функция `distance/2` сгененирует исключение, если мы передадим данные неправильного типа:

```elixir-iex
iex(2)> AtomTupleExample.distance({:point, 0, 0}, {:point, "5", "5"})
** (ArithmeticError) bad argument in arithmetic expression: 0 - "5"
```

или если передадим агрументы, которые не совпадают по шаблону:

```elixir-iex
iex(2)> AtomTupleExample.distance({0, 0}, {5, 5})
** (FunctionClauseError) no function clause matching in AtomTupleExample.distance/2
```

Так же будет себя вести и функция `point_inside_figure?/2`, мы это уже видели:

```elixir-iex
iex(2)> AtomTupleExample.point_inside_figure?({:point, 3, 3}, {:triangle, {:point, 0, 0}, {:point, 5, 5}, {:point, 0, 5}})
** (FunctionClauseError) no function clause matching in AtomTupleExample.point_inside_figure?/2
```

Добавим такие тесты:

```elixir
  test "invalid arguments for distance" do
    assert_raise FunctionClauseError, fn -> distance({0, 0}, {0, 5}) end
  end

  test "invalid arguments for inside figure" do
    assert_raise ArithmeticError, fn ->
      point_inside_figure?({:point, 1, 1}, {:circle, {:point, "5", "5"}, 10})
    end
  end
```

В этих тестах используются анонимные функции. Мы будем изучать их позже. Пока что важно знать, что это позволяет тесту перехватить исключение и проверить его тип.

Запустим все тесты:

```shell
$ elixir lib/atom_tuple_example.exs
.....
Finished in 0.1 seconds (0.1s on load, 0.00s async, 0.02s sync)
5 tests, 0 failures

Randomized with seed 242184
```
