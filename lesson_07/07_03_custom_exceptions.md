# Пользовательские типы исключений

Эликсир позволяет создавать собственные типы исключений.

Представим себе, что у нас есть некий сервис с HTTP API. Он принимает входящий запрос в виде JSON объекта, и валидирует его по JSON схеме. Если запрос не соответствует схеме, то генерируется исключение пользовательского типа, содержащее название схемы.

```
> c "07_03_custom_exception.exs"
> alias Lesson_07.Task_03_CustomException, as: L

> L.validate(%{})
** (SchemaValidationError) object doesn't match schema "my_request.json"
    07_03_custom_exceptions.exs:4: Lesson_07.Task_03_CustomException.validate/1

> L.validate_and_rescue(%{})
validation failed, schema name is my_request.json
:ok
```

TODO: два вида исключений и обработка их в одном try..rescue
и сюда же добавить RuntimeError
