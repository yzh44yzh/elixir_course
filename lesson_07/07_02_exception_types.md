# Виды исключений

## throw и catch

Если вы уже изучали Эликсир, то, вероятно, заметили, что кроме **raise** есть еще один способ сгенерировать исключение -- **throw**. 

```
> raise "something went wrong"
** (RuntimeError) something went wrong

> throw "something went wrong"
** (throw) "something went wrong"
```

А кроме **rescue** есть еще один способ перехватить исключение -- **catch**. 
```
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

Ситуация еще сложнее, чем кажется, потому что у нас не два, а четыре вида исключений. Три вида исключений идут из Эрланг, и четвертый вид добавляет Эликсир.

Виды исключений в Эрланг:
- :throw
- :error
- :exit

Исключение **:throw** генерируется вызовом функции throw/1:
```
> throw(:some_error)
** (throw) :some_error
```
Оно предназначено для разработчика, чтобы генерировать и обрабатывать ошибки на уровне бизнес логики.

Исключение **:error** генерируется вызовом функции :erlang.error/1:
```
> :erlang.error(:some_error)
** (ErlangError) Erlang error: :some_error
```
Это ошибка ниже уровня бизнес логики, и она должна приводить к аварийному завершению потока. По соглашению, разработчик может генерировать такие ошибки, но не должен их перехватывать.

Наконец, исключение **:exit** относится к многопоточному программированию, и лежит в основе взаимодействия потоков между собой. Эту тему мы будем изучать позже. По соглашению, разработчик не должен ни генерировать, ни перехватывать такие ошибки. 

TODO: откуда эти соглашения? Официальная дока? Книга Хеберта? Книга Амстронга? Другая?

Исключение **raise**, которое добавляет Эликсир, это надстройка над **:error**. Что идет вразрез с соглашениями, принятыми в Эрланг. Было бы идеологически правильно делать **raise** на базе **:throw**. 

Итак, у нас есть четыре вида исключений, и два способа их перехватывать. Давайте посмотрим, как работает все это вместе:

```
> c "07_02_exception_types.exs"
[Lesson_07.Task_02_ExceptionTypes]
> alias Lesson_07.Task_02_ExceptionTypes, as: L
Lesson_07.Task_02_ExceptionTypes

> L.try_resque(:raise)
rescue from %RuntimeError{message: "something went wrong"}
:ok
> L.try_resque(:throw)
** (throw) :something_went_wrong
    07_01_exception.exs:20: Lesson_07.Task_01_Exception.generate_exception/1
    07_01_exception.exs:5: Lesson_07.Task_01_Exception.try_resque/1
> L.try_resque(:error)
rescue from %ErlangError{original: :something_went_wrong}
:ok
> L.try_resque(:exit) 
** (exit) :something_went_wrong
    07_01_exception.exs:22: Lesson_07.Task_01_Exception.generate_exception/1
    07_01_exception.exs:5: Lesson_07.Task_01_Exception.try_resque/1

> L.try_catch(:raise)
catch error %RuntimeError{message: "something went wrong"}
:ok
> L.try_catch(:throw)
catch throw :something_went_wrong
:ok
> L.try_catch(:error)
catch error :something_went_wrong
:ok
> L.try_catch(:exit) 
catch exit :something_went_wrong
:ok
```

Мы видим, что **rescue** перехватывает **raise** и **:error**, но не может перехватить **:throw** и **:exit**. И мы видим, что **catch** перехватывает все виды исключений.

Чесно говоря, задумка авторов Эликсир мне не понятна. Поверх существующей системы исключений, уже достаточно сложной, они добавли свою, чем еще больше усложнили ситуацию. Возможно они хотели скрыть от разработчика систему из Эрланг, и дать ему только **raise** и **rescue**. Но это не сработало, потому что остальные виды исключений никуда не делись, и знать про них нужно. Они могут генерироваться рантаймом или сторонними библиотеками (а многие библиотеки Эликсир являются обертками над библиотеками Эрланг).


## timeout on GenServer call

Тут мы забегаем вперед, потому что gen_server еще не рассматривали.

TODO: показать, что raise это не ловит, а catch ловит.

TODO: Посмотреть, как это работает в Erl 24. 
посмотреть change log erl 24.

похоже в 24-м эрланге gen_server:call на timeout уже не бросает исключение, а возвращает {error, timeout}
