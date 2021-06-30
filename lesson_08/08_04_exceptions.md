# Решение 3. Использование исключений.

Шаблонность кода вызвана тем, что результат каждого вызова нужно проверить на ошибку. Добавим модуль BookShop.ValidatorEx, чтобы его функции сообщали об ошибках через исключения, а не через возвращаемое значение.

```
iex(2)> Solution3.main()
{:error, "cat 'Tihon' is not found"}
iex(3)> Solution3.main()
{:error, "invalid data"}
iex(4)> Solution3.main()
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
iex(5)> Solution3.main()
{:error, "book 'Domain Modeling Made Functional' is not found"}
```

Получилось очень простое и короткое решение. Код описывает happy path, а вся обработка ошибок собрана в одном месте -- rescue.

Недостаток использования исключений в том, что в большом проекте становится сложно отслеживать путь выполнения кода. Зачастую нет связи между местами, где генерируются исключения, и местами, где они обрабатываются. 

В функциональном программировании такой подход не является каноничным, и есть альтернативы, которые мы рассмотрим дальше.


