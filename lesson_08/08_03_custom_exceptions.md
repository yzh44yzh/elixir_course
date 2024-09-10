# Пользовательские типы исключений

Эликсир позволяет создавать собственные типы исключений. Их имеет смысл применять если вы строите бизнес-логику и управление потоком выполения на исключениях.

В очередной раз повторю -- так делать не рекомендуется. Но если очень хочется, то можно.

Представим себе, что у нас есть некий сервис с HTTP API. Он принимает входящие запросы в виде JSON объекта, делает что-то полезное, и отвечает на запросы какими-то адекватными данными.

Но прежде чем начать делать что-то полезное и собирать данные для ответа нужно провалидировать запрос. Данные в запросе могут быть не валидными, их нужно проверить. Но ещё до того, как проверить данные, нужно проверить отправителя -- кто он и имеет ли он право делать такие запросы.

Итак, нам нужно сделать:
- аутентификацию отправителя,
- авторизацию отправителя,
- валидацию данных в запросе.

Для каждого шага мы сделаем отдельное пользовательское исключение, которое прервёт обработку запроса, если нужно, и позволит вернуть клиенту подходящее сообщение об ошибке.

Начнём с аутентификации. Клиент передаёт либо некий токен, либо логин и пароль. Если они не валидные, то мы генерируем исключение `AuthorizationError`:

```
  defmodule AuthenticationError do
    @enforce_keys [:type]
    defexception [:type, :token, :login]
  end
```

Пользовательские исключения очень похожи на структуры, только вместо `defstruct` мы используем `defexception`. В остальном всё то же самое -- заворачиваем их внутрь модуля и указываем набор аттрибутов. Имя модуля становится типом исключения.

Но это ещё не всё, нужно реализовать behaviour `Exception`, которое включает два callback:
- `@callback exception(term()) :: Exception.t()`
- `@callback message(Exception.t()) :: String.t()`

Первый callback создаёт экземпляр исключения, второй создаёт текстовое представление для конкретного экземпляра.

В целом модуль выглядит так:

```
  defmodule AuthenticationError do
    @enforce_keys [:type]
    defexception [:type, :token, :login]

    @impl true
    def exception({auth_type, data}) do
      case auth_type do
        :token -> %AuthenticationError{type: auth_type, token: data}
        :login -> %AuthenticationError{type: auth_type, login: data}
      end
    end

    @impl true
    def message(error) do
      case error.type do
        :token -> "AuthenticationError: invalid token"
        :login -> "AuthenticationError: invalid login"
      end
    end
  end
```

Здесь `@behaviour Exception` явно указывать не нужно, оно подставляется не явно.

После того, как мы узнали, кто отправил запрос, мы проверяем, есть ли у клиента право выполнять такие запросы. Если нет, то генерируем исключение `AuthorizationError`:

```
  defmodule AuthorizationError do
    @enforce_keys [:role, :action]
    defexception [:role, :action]

    @impl true
    def exception({role, action}) do
      %AuthorizationError{role: role, action: action}
    end

    @impl true
    def message(error) do
      "AuthorizationError: role '#{error.role}' is not allowed to do action '#{error.action}'"
    end
  end
```

Ну и на третьем шаге проверяем данные. Если они не соответствуют схеме, то генерируем исключение `SchemaValidationError`:

```
  defmodule SchemaValidationError do
    @enforce_keys [:schema_name]
    defexception [:schema_name]

    @impl true
    def exception(schema_name) do
      %SchemaValidationError{schema_name: schema_name}
    end

    @impl true
    def message(error) do
      "SchemaValidationError: data does not match to schema '#{error.schema_name}'"
    end
  end
```

Теперь построим логику обработки запроса на этих исключениях:

```
  def handle(request) do
    try do
      authorize(request)
      authenticate(request)
      validate(request)
      result = do_something_useful(request)
      {200, result}
    rescue
      error in [M.AuthenticationError, M.AuthorizationError] ->
        {403, Exception.message(error)}

      error in [M.SchemaValidationError] ->
        {409, Exception.message(error)}

      error ->
        IO.puts(Exception.format(:error, error, __STACKTRACE__))
        {500, "internal server error"}
    end
  end
```

Для разных видов исключений мы возвращаем разные HTTP Status и сообщение об ошибке.

Мы подразумеваем, что функции `authorize`, `authenticate` и `validate` делают свою работу правильно. Но сейчас мы просто сделаем заглушки:

```
  def authorize(request) do
    case request.token do
      "aaa" -> :ok
      "bbb" -> :ok
      _ -> raise M.AuthenticationError, {:token, request.token}
    end
  end

  def authenticate(request) do
    case request.token do
      "aaa" -> :ok
      _ -> raise M.AuthorizationError, {:guest, :reconfigure}
    end
  end

  def validate(request) do
    case Map.has_key?(request.data, :a) do
      true -> :ok
      false -> raise M.SchemaValidationError, "some-schema.json"
    end
  end
```

Обработка валидного запроса будет в функции `do_something_useful`, которая сейчас тоже представлена заглушкой:

```
  def do_something_useful(%{data: %{a: 100}}) do
    raise "something happened"
  end

  def do_something_useful(request) do
    request.data.a
  end
```

Наконец, подготовим несколько разных запросов, которые будут попадать в разные ветки кода:

```
  def request1(), do: %{token: "aaa", data: %{a: 42}}

  def request2(), do: %{token: "bbb", data: %{a: 42}}

  def request3(), do: %{token: "aaa", data: %{b: 42}}

  def request4(), do: %{token: "ccc", data: %{a: 42}}

  def request5(), do: %{token: "aaa", data: %{a: 100}}
```

Теперь всё это запустим и посмотрим, как оно работает.

Первый запрос выполняется успешно:

```
iex(1)> alias CustomExceptionExample, as: C
CustomExceptionExample
iex(2)> C.request1() |> C.handle()
{200, 42}
```

Второй запрос не проходит авторизацию:

```
iex(3)> C.request2() |> C.handle()
{403, "AuthorizationError: role 'guest' is not allowed to do action 'reconfigure'"}
```

Третий запрос не проходит валидацию по схеме:

```
iex(4)> C.request3() |> C.handle()
{409, "SchemaValidationError: data does not match to schema 'some-schema.json'"}
```

Четвёртый запрос не проходит аутентификацию:

```
iex(5)> C.request4() |> C.handle()
{403, "AuthenticationError: invalid token"}
```

Пятый запрос вызывает `RuntimeError`:

```
iex(6)> C.request5() |> C.handle()
** (RuntimeError) something happened
    custom_exceptions.exs:57: CustomExceptionExample.do_something_useful/1
    custom_exceptions.exs:19: CustomExceptionExample.handle/1
    ...
```

Пользовательские типы исключений привычны для разработчиков, которые приходят в ФП из других языков. Позже мы рассмотрим, что характерно для ФП.
