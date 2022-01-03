# Пользовательские типы исключений

Эликсир позволяет создавать собственные типы исключений.

Представим себе, что у нас есть некий сервис с HTTP API. Он принимает входящий запрос в виде JSON объекта, и валидирует его по JSON схеме. Если запрос не соответствует схеме, то генерируется исключение пользовательского типа SchemaValidationError, содержащее название схемы:

```
> c "lib/custom_exception.exs"
> alias Lesson_08.CustomException, as: L

> L.validate(1)
** (SchemaValidationError) object doesn't match schema "my_request.json"
lib/custom_exceptions.exs:5: Lesson_08.CustomException.validate/1
```

И добавим еще одно пользовательское исключение AuthorizationError:

```
> L.do_action(2)
** (AuthorizationError) user with role "guest" doesn't have permission to do action "modify"
    lib/custom_exceptions.exs:6: Lesson_08.CustomException.validate/1
> L.validate(3)
:ok
```

Пользовательские исключения определяются внутри модуля. Это похоже на то, как определяются struct. Для создания экземпляра исключения используется функция **exception/1**. Мы не вызваем эту функцию напрямую, а **raise** вызывает ее опосредовано.


Реализуем некую имитацию обработки запроса в АПИ:

```
iex(30)> L.handle(1)
validation failed, schema name is my_request.json
:error
iex(31)> L.handle(2)
authorization failed, user with role "guest" doesn't have permission to do action "modify"
:error
iex(32)> L.handle(3)
:ok
=
```

В блоке **rescue** мы можем определить тип исключения и по разному обработать разные типы. Мы так же можем обращаться к аттрибутам исключения.

Это довольно типичный код, где обработка ошибок уровня бизнес-логики реализована на базе пользовательских исключений. 

Как я уже упоминал, в функциональном программировании такой способ считается плохим тоном. Несмотря на это, он используется довольно широко, потому что привычен для разработчиков, приходящих в ФП из других языков.

Позже мы рассмотрим другие способы, характерные для ФП.

