# Пользовательские типы исключений

Эликсир позволяет создавать собственные типы исключений.

Представим себе, что у нас есть некий сервис с HTTP API. Он принимает входящий запрос в виде JSON объекта и валидирует его по JSON схеме. Если запрос не соответствует схеме, то генерируется исключение пользовательского типа SchemaValidationError, содержащее название схемы:

```
iex(1)> c "lib/custom_exceptions.exs"
iex(2)> alias CustomExceptionExample, as: E
iex(3)> E.validate(E.request3())
** (Model.SchemaValidationError) data is not match to schema 'some-schema.json'
    lib/custom_exceptions.exs:52: CustomExceptionExample.validate/1
```

И добавим еще одно пользовательское исключение AuthorizationError:

```
iex(4)> E.authorize(E.request3())
:ok
iex(5)> E.authorize(E.request4())
** (Model.AuthenticationError) invalid token
    lib/custom_exceptions.exs:38: CustomExceptionExample.authorize/1
```

TODO AuthenticationError

Пользовательские исключения определяются внутри модуля. Это похоже на то, как определяются struct. Для создания экземпляра исключения используется функция **exception/1**. Мы не вызваем эту функцию напрямую, а **raise** вызывает ее опосредовано.

Реализуем некую имитацию обработки запроса в АПИ:

```elixir-iex
iex(6)> E.handle(E.request2())
{403, "role 'guest' is not allowed to do action 'reconfigure'"}
iex(7)> E.handle(E.request3())
{409, "data is not match to schema 'some-schema.json'"}
iex(8)> E.handle(E.request4())
{403, "invalid token"}
iex(9)> E.handle(E.request1())
{200, 42}
```

В блоке **rescue** мы можем определить тип исключения и по разному обработать разные типы. Мы так же можем обращаться к аттрибутам исключения.

Это довольно типичный код, где обработка ошибок уровня бизнес-логики реализована на базе пользовательских исключений. 

Как я уже упоминал, в функциональном программировании такой способ считается плохим тоном. Несмотря на это, он используется довольно широко, потому что привычен для разработчиков, приходящих в ФП из других языков.

Позже мы рассмотрим другие способы, характерные для ФП.
