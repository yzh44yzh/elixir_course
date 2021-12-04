defmodule MyCoolApp.ShardingAgent do

  use Agent, restart: :permanent

  def start_link({agent_name, state}) do
    IO.puts("init ShardingAgent #{agent_name} #{inspect state}")
    Agent.start(fn () -> state end, [name: agent_name])
  end

  def find_node(agent_name, shard_num) do
    Agent.get(agent_name, fn(state) -> do_find_node(state, shard_num) end)
  end

  defp do_find_node(state, shard_num) do
    Enum.reduce(state, {:error, :not_found},
      fn
        (_, {:ok, res}) -> {:ok, res}
        ({min_shard, max_shard, node_name}, res) ->
          if shard_num >= min_shard and shard_num <= max_shard do
            {:ok, node_name}
          else
            res
          end
      end)
  end
  
end
