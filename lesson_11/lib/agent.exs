defmodule MyAgent do

  defmodule OnlineUsers do

    def start() do
      Agent.start(fn() -> [] end, name: :online_users)
    end

    def add_user(username) do
      num_shards = Lesson.ShardingInfo.get_num_shards()
      shard = :erlang.phash2(username, num_shards)
      {:ok, node} = Lesson.ShardingInfo.get_node(shard)
      user = {username, shard, node}
      Agent.update(:online_users, fn(users) -> [user | users] end)
    end

    def get_users() do
      Agent.get(:online_users, fn(users) -> users end)
    end

    def is_online?(username) do
      Agent.get(:online_users, fn(users) -> :lists.keyfind(username, 1, users) end)
    end
    
  end

  defmodule ShardingInfo do

    def start() do
      shards = [
        {0, 7, "node-1"},
        {8, 15, "node-2"},
        {16, 23, "node-3"},
        {24, 31, "node-4"}
      ]
      state = %{
        shards: shards,
        num_shards: 32
      }
      Agent.start(fn() -> state end, name: :sharding_info)
    end

    def get_num_shards() do
      Agent.get(:sharding_info, fn(%{num_shards: num_shards}) -> num_shards end)
    end

    def get_node(shard) do
      Agent.get(:sharding_info, fn(%{shards: shards}) -> find_node(shard, shards) end)
    end

    defp find_node(shard, state) do
      case Enum.reduce(state, {shard, :not_found}, &check_node/2) do
        {:ok, node} -> {:ok, node}
        {_shard, :not_found} -> {:error, :not_found}
      end
    end

    defp check_node(_, {:ok, _node} = acc), do: acc
    defp check_node({from_shard, to_shard, node}, {shard, :not_found}) do
      if shard >= from_shard and shard <= to_shard do
        {:ok, node}
      else
        {shard, :not_found}
      end
    end
        
  end
  
end
