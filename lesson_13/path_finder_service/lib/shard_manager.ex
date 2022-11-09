defmodule ShardManager do

  use Agent

  require Logger
  
  def start_link({name, state}) do
    Logger.info("ShardManager.start #{name} with state #{inspect state}")
    Agent.start_link(fn () -> state end, [name: name])
  end

  def find_node(name, shard_num) do
    Agent.get(name, fn(state) -> do_find_node(state, shard_num) end)
  end

  def reshard(nodes, num_shards) do
    num_nodes = length(nodes)
    shards_per_node = div(num_shards, num_nodes)
    {_, new_state} =
      Enum.reduce(nodes, {0, []},
        fn (node, {from_shard, acc}) ->
          to_shard = from_shard + shards_per_node
          to_shard = if to_shard > num_shards, do: num_shards, else: to_shard
          {to_shard, [{from_shard, to_shard - 1, node} | acc]}
        end)
    Agent.update(:sharding_info, fn(_old_state) -> new_state end)
    new_state
  end

  # it works inside Agent process
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
