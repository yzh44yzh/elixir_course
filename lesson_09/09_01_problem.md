# Композиция функций

Абстракция -- мощный инструмент в руках разработчика и архитектора. Абстракция позволяет скрыть сложности реализации за удобным АПИ -- фасадом. Назовём это компонентом.

Но как понять, получился ли компонент хорошим? Это становится понятно, когда мы начнём соединять разные компоненты друг с другом, комбинировать их в разных сочетаниях. Если компоненты легко сочетаются и комбинируются, то это хорошая абстракция.

В ООП компонент реализуется через объект, а в ФП через функцию или набор функций -- модуль.

Важно научиться делать функции такими, чтобы их было удобно компоновать с другими функциями. А для этого нужно знать, какие способы компоновки вообще существуют.

В тривиальном случае, когда одна функция возвращает такое значение, которое может быть аргументом другой функции, соединить их не составляет труда:

```elixir
@spec f1(integer()) :: integer()
def f1(a) do
  a + 1
end

@spec f2(integer()) :: integer()
def f2(a) do
  a + 10
end

42 |> f1 |> f2
```

Однако тривиальный случай встречается не часто. Обычно приходится прикладывать дополнительные усилия.

Вход и выход функций не совпадает:

```elixir
@spec f3(integer()) :: {:ok, integer()}
def f3(a) do
  {:ok, a + 1}
end

42 |> f3 |> elem(1) |> f2
```

Выход может быть с ошибкой:

```elixir
def f4(a) do
  if a > 10 do
    {:ok, a + 1}
  else
    {:error, :below_limit}
  end
end

42 |> f4 |> elem(1) |> f2
# 53

2 |> f4 |> elem(1) |> f2
# ** (ArithmeticError) bad argument in arithmetic expression
```

Функция, помимо выхода, еще генерирует побочные эффекты:

```elixir
def f5(a) do
  drop_database()
  a + 1
end

42 |> f5 |> f2
```

Чем сложнее у функций вход-выход, чем больше побочных эффектов, тем сложнее делать их композицию.

В некоторых языках для этого придумывают сложные абстракции: аппликативные функторы, монады, монадные трансформеры. Этим особенно знаменит Хаскель.

Более прагматичные языки обходятся более простыми средствами. Рассмотрим, что можно сделать в Эликсир на примере конкретной задачи.


## Задача на композицию функций.

У нас есть книжный магазин для котов. Он принимает заказы и доставляет книги.

У магазина есть API для создания заказа.

На входе API принимает json-данные, содержащие информацию о коте-заказчике, его адрес и книги, которые кот хочет заказать.

Например:

```json
{
  "cat": "Tihon",
  "address": "Coolcat str 7/42 Minsk Belarus",
  "books": [
    {"title": "Domain Modeling Made Functional", "author": "Scott Wlaschin"},
    {"title": "Удовольствие от Х", "author": "Стивен Строгац"},
    {"title": "Distributed systems for fun and profit", "author": "Mikito Takada"}
  ]
}
```

На выходе из API мы имеем валидный бизнес-объект **Order**, который передается дальше в систему для обработки, или ошибку валидации.

Для проверки данных и создания валидного объекта Order нужно выполнить следующие шаги:
- проверить кота по имени;
- проверить его адрес;
- проверить каждую книгу в списке;
- создать Order.

Для этого у нас есть следующие функции:

```elixir
@spec validate_incoming_data(map()) :: {:ok, map()} | {:error, :invalid_incoming_data}

@spec validate_cat(binary()) :: {:ok, cat()} | {:error, :cat_not_found}

@spec validate_address(binary()) :: {:ok, address()} | {:error, :invalid_address}

@spec get_book(binary(), binary()) :: {:ok, Book.t} | {:error, {:book_not_found, binary()}}

@spec create_order(cat(), address(), [Book.t]) :: Order.t
```

У нас есть 3 функции **validate_**, которые могут вернуть успешный результат, либо ошибку. Есть четвертая функция **get_book/2**, которую нужно применить несколько раз к элементам списка. И, наконец, пятая функция **create_order/3**, которая всегда возвращает успешный результат.

Нужно выполнить композицию этих функций.
