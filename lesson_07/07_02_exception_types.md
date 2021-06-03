## throw / try..catch

```
try do
  incite(n)
catch
  :exit, code -> "Exited with code #{inspect code}"
  :throw, value -> "throw called with #{inspect value}"
  what, value -> "Caught #{inspect what} with #{inspect value}"
end
```

В некоторых книгах по Эликсир предлагают использовать throw как control flow, а raise не использовать так.
TODO: в каких?

The purpose of throws if to allow nonlocal returns.
There is no constracts such as break, continue, and return.
But using throws for control flow is hacky. You should avoid this technique.


## Зачем нужны 2 вида исключений?

Ситуация еще сложнее, чем кажется, потому что у нас не два, а четыре вида исключений.

3 из них идут из Эрланг (и там есть четкая концепция, как их применять):
- throw
- error
- exit

Как и в большинстве языков программирования, в эрланг есть исключения
и способ их перехватить и обработать. Но картина несколько усложняется
тем, что есть три типа исключений и три разных способа их генерировать.

**throw(Reason)** -- генерирует обычное исключение.
Чаще всего именно эту функцию используют разработчики.

**erlang:error(Reason)** -- генерирует фатальную ошибку,
восстановление после которой не подразумевается, и текущий поток
должен упасть.  Впрочем, это скорее соглашение, нежели техническое
отличие.  Перехватить и обработать это исключение все равно можно.

**exit(Reason)** -- генерирует системное сообщение. Мы это обсуждали в
11-м уроке "Обработка ошибок на низком уровне" и помним, что с помощью
системных сообщений реализуются связи между потоками.  Необходимость
вызывать exit/1 и вмешиваться в этот механизм возникает очень редко,
разве что в целях тестирования.


И один вид добавляет Эликсир
- raise

И есть два способа перехватывать исключения:
- try...rescue
- try...catch

catch идет из Эрланга, rescue добавлен в Эликсире.

В книгах обычно описывают 2 из 4х видов: raise и throw. И описывают оба способа перехвата, но не объясняют, зачем их два разных. И не объясняют существование эрланговских исключений, и какая концепция стоит за ними.

```
iex(1)> c "07_02_exception_types.exs"
[Lesson_07.Task_02_ExceptionTypes]
iex(2)> alias Lesson_07.Task_02_ExceptionTypes, as: L
Lesson_07.Task_02_ExceptionTypes
iex(3)> L.try_resque(:raise)
rescue from %RuntimeError{message: "something went wrong"}
:ok
iex(4)> L.try_resque(:throw)
** (throw) :something_went_wrong
    07_01_exception.exs:20: Lesson_07.Task_01_Exception.generate_exception/1
    07_01_exception.exs:5: Lesson_07.Task_01_Exception.try_resque/1
iex(4)> L.try_resque(:error)
rescue from %ErlangError{original: :something_went_wrong}
:ok
iex(5)> L.try_resque(:exit) 
** (exit) :something_went_wrong
    07_01_exception.exs:22: Lesson_07.Task_01_Exception.generate_exception/1
    07_01_exception.exs:5: Lesson_07.Task_01_Exception.try_resque/1
iex(5)> L.try_catch(:raise)
catch error %RuntimeError{message: "something went wrong"}
:ok
iex(6)> L.try_catch(:throw)
catch throw :something_went_wrong
:ok
iex(7)> L.try_catch(:error)
catch error :something_went_wrong
:ok
iex(8)> L.try_catch(:exit) 
catch exit :something_went_wrong
:ok
```


## timeout on GenServer call

Тут мы забегаем вперед, потому что gen_server еще не рассматривали.

TODO: показать, что raise это не ловит, а catch ловит.

TODO: Посмотреть, как это работает в Erl 24. 
посмотреть change log erl 24.

похоже в 24-м эрланге gen_server:call на timeout уже не бросает исключение, а возвращает {error, timeout}
