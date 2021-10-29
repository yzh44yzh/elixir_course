# Динамический супервизор

Мы создали статичное дерево супервизоров. Все процессы в нем запускаются на старте узла (виртуальной машины) и живут столько, сколько живет сам узел. 

Часто бывают нужны и короткоживущие процессы. Их можно запускать и под обычным супервизором с настройкой restart: `transient` или `temporary`. Но в Эликсире для этого есть специальный вид супервизора -- динамический супервизор [DynamicSupervisor](https://hexdocs.pm/elixir/1.12/DynamicSupervisor.html)

Рассмотрим его на примере задачи:

Допустим, наш сервис -- это один из многих в системе, реализованной в микросервисной архитектуре. В системе есть описаны правила, каким клиентам в какие сервисы разрешено делать запросы. Эти правила хранит сервис авторизации, откуда их получают все остальные сервисы, в том числе наш. 

Нам нужно реализовать процесс, который запрашивает данные в сервисе авторизации, и сохраняет их в некое локальное хранилище. Данные меняются редко, поэтому не нужно постоянно отслеживать изменения в них. А нужно загрузить их один раз на старте сервиса, и иногда, при получении определённого события загружать их повторно.

Нет необходимости постоянно держать долгоживущий процесс для такой задачи. Здесь лучше подойдет короткоживущий процесс, который выполнит свою работу и завершится. А при необходимости запустится снова, снова выполнит работу и завершится.

```
iex(1)> c "lib/dyn_sup.exs"
[Lesson_11, Lesson_11.AuthDataLoader, Lesson_11.AuthDataLoaderSup,
 Lesson_11.MyService]
iex(2)> Lesson_11.MyService.start()
{:ok, #PID<0.125.0>}
iex(3)> Lesson_11.MyService.update_auth_rules()
worker #PID<0.128.0> started with http://auth_service.some_cluster.data_center/rules
load data from http://auth_service.some_cluster.data_center/rules
{:ok, #PID<0.128.0>}
save data [:rule_1, :rule_2, :rule_3]
work done
iex(4)> Lesson_11.MyService.update_auth_rules()
worker #PID<0.130.0> started with http://auth_service.some_cluster.data_center/rules
load data from http://auth_service.some_cluster.data_center/rules
{:ok, #PID<0.130.0>}
save data [:rule_1, :rule_2, :rule_3]
work done
```




A DynamicSupervisor starts with no children. Instead, children are started on demand via start_child/2. When a dynamic supervisor terminates, all children are shut down at the same time, with no guarantee of ordering.

We also chose the :one_for_one strategy, which is currently the only available strategy for dynamic supervisors.

The difference is that the DynamicSupervisor expects the child specification at the moment start_child/2 is called, and no longer on the init callback. 

init options
:max_children - the maximum amount of children to be running under this supervisor at the same time. When :max_children is exceeded, start_child/2 returns {:error, :max_children}. Defaults to :infinity.

:extra_arguments - arguments that are prepended to the arguments specified in the child spec given to start_child/2. Defaults to an empty list.





start_child returns {:ok, pid} or {:error, {:already_started, pid}}
что является удобным способом получить pid существующего воркера или запустить нового, если нет существующего.
И это исключает race condition при попытке запустить воркера с одинаковым id из разных мест,
так как start_child сериализуется в одном процессе.
С другой стороны, это не очень эффективно, тк супервизор каждый раз делает попытку запуска нового процесса.

This type of supervisor allows you to create an arbitrary number of workers at runtime.


