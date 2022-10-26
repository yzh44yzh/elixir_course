defmodule Lesson do

  defmodule Session do
    @type t :: %Session{
      username: String.t(),
      num_shard: integer(),
      node_name: String.t()
    }
    
    defstruct [:username, :num_shard, :node_name]
  end

  defmodule SessionManager do
    @type state() :: [Session.t()]

    @spec add_session(pid(), String.t()) :: :ok
    def add_session(agent_pid, username) do
      {shard, node} = ShardManager.settle(username)
      session = %Session{username: username, num_shard: shard, node_name: node}
      Agent.update(agent_pid, fn(state) -> [session | state] end)
      :ok
    end

    @spec get_sessions(pid()) :: [Session.t()]
    def get_sessions(agent_pid) do
      Agent.get(agent_pid, fn(state) -> state end)
    end

    def get_session_by_name(agent_pid, name) do
      Agent.get(agent_pid, fn(state) -> find_session(state, name) end)
    end

    @spec start() :: pid()
    def start() do
      state = []
      Agent.start(fn() -> state end)
    end

    def stop(pid) do
      Agent.stop(pid)
    end

    defp find_session(sessions, name) do
      Enum.find(sessions, fn(session) -> session.username == name end)
    end
    
  end

  defmodule ShardManager do

    def start() do
      state = %{
        num_shards: 32,
        shards: [
          {0, 7, "node-1"},
          {8, 15, "node-2"},
          {16, 23, "node-3"},
          {24, 31, "node-4"}
        ]
      }
      
      Agent.start(fn() -> state end, name: :shard_manager)
    end

    @spec settle(String.t()) :: {integer(), String.t()}
    def settle(username) do
      num_shards = Agent.get(:shard_manager, fn(state) -> state.num_shards end)
      shard = :erlang.phash2(username, num_shards)
      {:ok, node} = get_node(shard)
      {shard, node}
    end

    def get_node(shard) do
      Agent.get(:shard_manager, fn(state) -> find_node(state.shards, shard) end)
    end

    defp find_node(state, shard) do
      Enum.reduce(state, {:not_found, shard}, &check_node/2)
    end

    defp check_node(_, {:ok, node}), do: {:ok, node}
    defp check_node({from_shard, to_shard, node}, {:not_found, shard}) do
      if from_shard <= shard and to_shard >= shard,
        do: {:ok, node},
        else: {:not_found, shard}
    end
  end
  
end

