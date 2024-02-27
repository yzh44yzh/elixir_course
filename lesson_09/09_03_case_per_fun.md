# Решение 2. Каждый case в отдельной функции.

```elixir-iex
iex(27)> data = BookShop.test_data()
iex(28)> Solution2.handle(data)
{:error, {:book_not_found, "Удовольствие от Х"}}
iex(29)> Solution2.handle(data)
{:error, :invalid_incoming_data}
iex(30)> Solution2.handle(data)
{:error, :cat_not_found}
iex(31)> Solution2.handle(data)
{:ok,
 %BookShop.Order{ 
   books: [
     %BookShop.Book{
       author: "Scott Wlaschin",
       id: "ISBN 978-5-00057-917-6",
       title: "Domain Modeling Made Functional"
     },
     %BookShop.Book{
       author: "Стивен Строгац",
       id: "ISBN 978-5-00057-917-6",
       title: "Удовольствие от Х"
     },
     %BookShop.Book{
       author: "Mikito Takada",
       id: "ISBN 978-5-00057-917-6",
       title: "Distributed systems for fun and profit"
     }
   ],
   customer: {:cat, "Tihon"},
   shipping_address: {:address, "Coolcat str 7/42 Minsk Belarus"}
 }}
```

У нас получилось 4 небольшие функции, вызывающие друг друга по очереди. И некий общий State, который проходит через все эти вызовы. State нужен, чтобы накапливать промежуточные результаты и передавать их дальше.

Каждая функция маленькая и понятная. Тут легко добавить два, пять, десять, сколько угодно новых шагов валидации. Легко менять их местами.

dialyzer по-прежнему контролирует правильность композиции. Но за правильностью использования State разработчику придется следить самому. Тут появляются возможности для ошибок.

Кроме того, функции похожи, они повторяют одинаковый шаблон. И это наводит на мысль, что можно что-то обобщить, сократить количество кода.
