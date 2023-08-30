# Классы исключений

## throw и catch

Если вы уже изучали Эликсир, то, вероятно, заметили, что кроме **raise** есть еще один способ сгенерировать исключение -- **throw**. 

```
> raise "something went wrong"
** (RuntimeError) something went wrong

> throw "something went wrong"
** (throw) "something went wrong"
```

А кроме **rescue** есть еще один способ перехватить исключение -- **catch**. 

```elixir
try do
  raise "something went wrong"
rescue
  error -> ...
end

try do
  throw "something went wrong"
catch
  err_type, error -> ...
end
```

Почему так? Чтобы ответить на этот вопрос придется познакомиться с исключениями в Эрланг.

## Исключения в Эрланг

На самом деле в BEAM встроены не два, а три класса исключений:
- :throw
- :error
- :exit

Исключения класса **:throw** генерируется вызовом функции throw/1:

```elixir-iex
> throw(:some_error)
** (throw) :some_error
```

В Эрланг их обычно используют, чтобы генерировать и обрабатывать ошибки на уровне бизнес логики.

Исключения класса **:error** возникают при ошибках в рантайме, таких как :badmatch, :badarith и др. Их можно сгененировать вызовом функции :erlang.error/1:

```elixir-iex
> :erlang.error(:some_error)
** (ErlangError) Erlang error: :some_error
```

Обе функции, throw и error, принадлежат модулю :erlang. Однако throw может вызываться напрямую без имени модуля, а error нет. Так сделано потому, что в Эрланг не рекомендуется пользоваться функцией error и явно создавать ошибки этого класса.

Наконец, исключения класса **:exit** относятся к многопоточному программированию, и лежат в основе взаимодействия потоков между собой. Эту тему мы будем изучать позже. 

По соглашению, принятому в Эрланге:
- throw - для разработчиков, можно создавать кастомные, бросать и перехватывать
- error - системные, можно бросать, нельзя перехватывать. К ним нужно относиться как к panic.
- exit - для межпроцессорного взаимодейсвия, не нужно ни бросать ни перехватывать, если только вы не создаёте новые способы взаимодействия процессов.

Разработчики Эликсир проигнорировали соглашения, принятые в Эрланг. Исключение **raise**, которое добавляет Эликсир -- это надстройка над классом **:error**. Эликсир оборачивает рантайм ошибки Эрланг в свои собственные объекты:

|| Эрланг || Эликсир ||
| :badmatch | MatchError |
| :badarith | ArithmeticError |
| :undef | UndefinedFunctionError |

Итак, у нас есть три класса исключений и два способа их перехватывать. Давайте посмотрим, как это работает всё вместе:

```elixir-iex
> c "lib/exception_types.exs"
[ExceptionTypesExample]
> alias ExceptionTypesExample, as: E
> E.try_resque(:raise)
rescue from %RuntimeError{message: "something went wrong"}
:ok
> E.try_resque(:throw)
** (throw) :something_went_wrong
    lib/exception.exs:20: ExceptionTypesExample.generate_exception/1
    lib/exception.exs:5: ExceptionTypesExample.try_resque/1
> E.try_resque(:error)
rescue from %ErlangError{original: :something_went_wrong}
:ok
> E.try_resque(:exit) 
** (exit) :something_went_wrong
    lib/exception.exs:22: ExceptionTypesExample.generate_exception/1
    lib/exception.exs:5: ExceptionTypesExample.try_resque/1

> E.try_catch(:raise)
catch error %RuntimeError{message: "something went wrong"}
:ok
> E.try_catch(:throw)
catch throw :something_went_wrong
:ok
> E.try_catch(:error)
catch error :something_went_wrong
:ok
> E.try_catch(:exit)
catch exit :something_went_wrong
:ok
```

Мы видим, что **rescue** перехватывает **raise** и **:error**, но не может перехватить **:throw** и **:exit**. И мы видим, что **catch** перехватывает все виды исключений.

Вероятно авторы Эликсир хотели скрыть от разработчика систему исключений из Эрланг и дать ему только **raise** и **rescue**. Но это не сработало, потому что остальные классы исключений никуда не делись, и знать про них нужно. Они могут генерироваться рантаймом или сторонними библиотеками (а многие библиотеки Эликсир являются обертками над библиотеками Эрланг).

## timeout on GenServer call

Мы еще не рассматривали GenServer, будем делать это позже. Но это хороший пример того, как rescue может неожиданно вас подвести. 

Речь идет о синхронном взаимодействии между двумя потоками, где один поток выполняет роль клиента и отправляет запрос, а другой поток выполняет роль сервера и отвечает на этот запрос. Если обработка запроса длится слишком долго (по умолчанию лимит составляет 5 секунд), то генерируется исключение класса :exit. 

```elixir-iex
> c "lib/gen_server_timeout.exs"
[GenServerTimeoutExample, MyServer]
> alias GenServerTimeoutExample, as: E
> E.start_server()

> E.normal_request()
{:ok, 42}

> E.long_request_with_resque()
** (exit) exited in: GenServer.call(MyServer, :long_request, 5000)
    ** (EXIT) time out
    (elixir 1.11.3) lib/gen_server.ex:1027: GenServer.call/3
    lib/gen_server_timeout.exs:13: GenServerTimeoutExample.long_request_with_resque/0

> E.long_request_with_catch() 
Got error exit {:timeout, {GenServer, :call, [MyServer, :long_request, 5000]}}
```

Как мы видим, rescue не перехватывает такое исключение, а catch перехватывает.

Такое вполне может случиться в реальной жизни даже если вы не используете GenServer в своем проекте. GenServer там все равно есть, в стандартных и в сторонних библиотеках. Например, в библиотеке Logger. Вы же будете писать логи в своем проекте? :)
