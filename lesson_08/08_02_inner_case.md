# Решение 1. Вложенные case.

```
$ iex -S mix
...
iex(1)> Solution1.main()
{:error, {:book_not_found, "Distributed systems for fun and profit"}}

iex(2)> Solution1.main()
{:error, :invalid_incoming_data}

iex(3)> Solution1.main()
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
}
```

Здесь получилось 4 уровня вложенности. Пока что это не так страшно. Но что, если понадобится добавить еще один шаг валидации? Два шага? Десять? Или переставить некоторые шаги местами?

Такой код -- явный пример, как не надо делать. Тем не менее, тут есть пару плюсов. Во-первых, он работает. Во-вторых, он хорошо проверяется dialyzer.

