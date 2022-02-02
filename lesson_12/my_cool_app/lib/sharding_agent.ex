defmodule MyCoolApp.ShardingAgent do

  use Agent, restart: :permanent

  def start_link({agent_name, state}) do
    Agent.start_link(fn() -> state end, name: agent_name)
  end

  def find_node(agent_name, shard_num) do
    Agent.get(agent_name, fn(state) -> find_node_(state, shard_num) end)
  end

  # it works inside Agent process
  defp find_node_(state, shard_num) do
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
