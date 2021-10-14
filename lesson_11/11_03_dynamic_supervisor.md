# Dynamic supervisor

Opposite to the previous Supervisor we defined, the children are not known upfront, but they are rather started dynamically. 
The DynamicSupervisor does not expect a list of children during initialization; instead each child is started manually via DynamicSupervisor.start_child/2

Since a DynamicSupervisor does not define any children during initialization, the DynamicSupervisor also allows us to skip the work of defining a whole separate module with the usual start_link function and the init callback. Instead, we can define a DynamicSupervisor directly in the supervision tree, by giving it a name and a strategy.

```
  def init(:ok) do
    children = [
      {KV.Registry, name: KV.Registry},
      {DynamicSupervisor, name: KV.BucketSupervisor, strategy: :one_for_one}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
```
We also chose the :one_for_one strategy, which is currently the only available strategy for dynamic supervisors.


```
DynamicSupervisor.start_link(name: __MODULE__, strategy: :one_for_one)
DynamicSupervisor.start_child(__MODULE__, {WorkerModule, worker_name})
WorkerModule.child_spec(_) do
  %{id: ..., start: {WorkerModule, :start_link, []}} 
end
```

start_child returns {:ok, pid} or {:error, {:already_started, pid}}
что является удобным способом получить pid существующего воркера или запустить нового, если нет существующего.
И это исключает race condition при попытке запустить воркера с одинаковым id из разных мест,
так как start_child сериализуется в одном процессе.
С другой стороны, это не очень эффективно, тк супервизор каждый раз делает попытку запуска нового процесса.

This type of supervisor allows you to create an arbitrary number of workers at runtime.

A DynamicSupervisor encapsulates what used to be the :simple_one_for_one strategy in regular supervisors.

```
defmodule Duper.WorkerSupervisor do
  use DynamicSupervisor

  @me WorkerSupervisor

  def start_link(_) do
    DynamicSupervisor.start_link(__MODULE__, :no_args, name: @me)
  end

  def init(:no_args) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def add_worker() do
    {:ok, _pid} = DynamicSupervisor.start_child(@me, Duper.Worker)
  end
end
```

Использование **simple_one_for_one** стратегии -- это особый случай,
когда нам нужно иметь большое количество потоков: десятки и сотни.

При использовании этой стратегии супервизор может иметь потомков
только одного типа. И, соответственно, должен указать только одну
child specitication.
TODO: это не актуально для эликсира?

