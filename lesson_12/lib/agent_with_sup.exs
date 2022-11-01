defmodule Lesson_12 do

  defmodule ShardManager do

    use Agent, restart: :permanent

    def start_link({agent_name, state}) do
      Agent.start_link(fn () -> state end, [name: agent_name])
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


  def start() do
    state = [
      { 0, 11, "Node-1"},
      {12, 23, "Node-2"},
      {24, 35, "Node-3"},
      {36, 47, "Node-4"}
    ]
    child_spec = [
      {ShardManager, {:agent_1, state}}
    ]
    Supervisor.start_link(child_spec, strategy: :one_for_all)
  end

  
  def start_2_agents() do
    state_1 = [
        {0, 4, "Node-1"},
        {5, 9, "Node-2"}
    ]
    state_2 = [
        { 0,  9, "Node-1"},
        {10, 19, "Node-2"},
        {20, 29, "Node-3"}
    ]

    child_spec = [
      %{
        id: :agent_a,
        start: {ShardManager, :start_link, [{:agent_a, state_1}]}
      },
      %{
        id: :agent_b,
        start: {ShardManager, :start_link, [{:agent_b, state_2}]}
      }
    ]
    Supervisor.start_link(child_spec, strategy: :one_for_all)
  end
  
end
