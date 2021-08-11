
https://hexdocs.pm/elixir/1.12/Agent.html

https://elixir-lang.org/getting-started/mix-otp/agent.html

- придумать пример

# Agent

An agent is a background process that maintains state.

The initial state is set by a function we pass in when we start the agent.

We can interrogate (опросить) the state using Agent.get, 
passing it the agent descriptor and a function. 
The agent runs the function on its current state and returns the result.

We can also use Agent.update to change the state held by an agent. 
As with the get operator, we pass in a function. 
Unlike with get , the function’s result becomes the new state.

```
iex> { :ok, count } = Agent.start(fn -> 0 end)
{:ok, #PID<0.69.0>}
iex> Agent.get(count, &(&1))
0
iex> Agent.update(count, &(&1+1))
:ok
iex> Agent.update(count, &(&1+1))
:ok
iex> Agent.get(count, &(&1))
2
```

_странное АПИ, для start и get функция не нужна, нужна только для update_
Для get нужна, чтобы не копировать стейт из одного процесса в другой.
Для start действительно непонятно, зачем функция.

We can also give agents a local or global name and access them using this name.

```
iex> Agent.start(fn -> 1 end, name: Sum)
{:ok, #PID<0.78.0>}
iex> Agent.get(Sum, &(&1))
1
```
In this case we exploit the fact that an uppercase bareword in Elixir 
is converted into an atom with the prefix Elixir., 
so when we say Sum it is actually the atom :Elixir.Sum.

```
defmodule Frequency do
  def start_link do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end
  def add_word(word) do
    Agent.update(__MODULE__, fn map ->
      Map.update(map, word, 1, &(&1+1))
    end)
  end
  def count_for(word) do
    Agent.get(__MODULE__, fn map -> map[word] end)
  end
  def words do
    Agent.get(__MODULE__, fn map -> Map.keys(map) end)
  end
end
```
_ок, функция для get полезна, чтобы не копировать весь state между процессами, а только часть. Функция для start не понятно, зачем нужна._

Agents and tasks run as OTP servers, so they are easy to distribute — 
just give our agent a globally accessible name.

One problem with Agents is that they completely open the process's state.
Wrap Agent in a dedicated module.
