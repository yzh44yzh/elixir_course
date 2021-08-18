# Agent

[Agent](https://hexdocs.pm/elixir/1.12/Agent.html)

Agents are a simple abstraction around state.
Often in Elixir there is a need to share or store state that must be accessed from different processes or by the same process at different points in time.

глобальный state доступный по имени из любого процесса

простой пример для демонстрации АПИ
список присутствующих в чате 
  
```
{:ok, agent_pid} = Agent.start(fn () -> [] end)

Agent.update(agent_pid, fn (members) -> ["Bob" | members] end)
Agent.update(agent_pid, fn (members) -> ["Kate" | members] end)

Agent.get(agent_pid, fn (members) -> members end)

add_member = fn(name) ->
  Agent.update(agent_pid, fn (members) -> [name | members] end)
end

add_member.("John")

get_members = fn() ->
  Agent.get(agent_pid, fn (members) -> members end)
end

get_members.()

add_member.("Helen")
add_member.("Bill")
```

открытый state который может модифицировать кто угодно.
чтобы спрятать state и дать к нему доступ через АПИ, можно:
- обернуть Agent в свой модуль
- использовать GenServer

полезный пример с оборачиванием в модуль
шардниг, диапазон шард с матчингом на id ноды

```
iex(1)> c "10_02_sharding_agent.exs"
[Lesson_10.Task_02_Sharding]
iex(2)> alias Lesson_10.Task_02_Sharding, as: T
Lesson_10.Task_02_Sharding
iex(3)> T.start
:ok
iex(4)> T.find_node(1)
{:ok, "Node-1"}
iex(5)> T.find_node(10)
{:ok, "Node-1"}
iex(6)> T.find_node(12)
{:ok, "Node-2"}
iex(7)> T.find_node(30)
{:ok, "Node-3"}
iex(8)> T.find_node(300)
{:error, :not_found}
```

Зачем здесь Agent? В чем выгода? 

Состояние хранится только в одном экзепляре. Если бы мы не обернули состояние в Agent, то каждый процесс, который пользовался бы этим АПИ, имел бы полную копию состояния в своей памяти.

Если состояние меняется, то оно меняется только в одном месте. И после этого все процессы имеют доступ к свежей версии состояния.

TODO - добавить АПИ для изменения состояния

TODO - показать работу с агентом из двух разных процессов
  

