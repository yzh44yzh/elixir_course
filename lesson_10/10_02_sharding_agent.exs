defmodule Lesson_10.Task_02_Sharding do

  def start() do
    state = [
      { 0, 11, "Node-1"},
      {12, 23, "Node-2"},
      {24, 35, "Node-3"},
      {36, 47, "Node-4"}
    ]
    Agent.start(fn () -> state end, [name: :sharding_info])
    :ok
  end

  def find_node(shard_num) do
    Agent.get(:sharding_info, fn(state) -> find_node(state, shard_num) end)
  end

  # it works inside Agent process
  defp find_node(state, shard_num) do
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
