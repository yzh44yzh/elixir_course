defmodule Lesson_12 do

  defmodule MyApp do

    def start_sup_tree() do
      children = [
        Lesson_12.RootSup
      ]
      Supervisor.start_link(children, strategy: :one_for_one) 
    end
    
  end
  
  defmodule RootSup do
    use Supervisor

    def start_link(args) do
      Supervisor.start_link(__MODULE__, args, name: __MODULE__)
    end

    @impl true
    def init(_args) do
      children = [
        {Lesson_12.AgentSup, [:no_args]},
        {Lesson_12.PathFinder, [:no_args]}
      ]
      Supervisor.init(children, strategy: :one_for_all) 
    end
    
  end

  defmodule AgentSup do
    use Supervisor

    def start_link(args) do
      Supervisor.start_link(__MODULE__, args, name: __MODULE__)
    end

    @impl true
    def init(_args) do
      state_1 = [
        {0, 4, "Node-1"},
        {5, 9, "Node-2"}
      ]
      state_2 = [
        { 0,  9, "Node-1"},
        {10, 19, "Node-2"},
        {20, 29, "Node-3"}
      ]
      children = [
        %{
          id: :agent_a,
          start: {Lesson_12.ShardingAgent, :start_link, [{:agent_a, state_1}]}
        },
        %{
          id: :agent_b,
          start: {Lesson_12.ShardingAgent, :start_link, [{:agent_b, state_2}]}
        }
      ]
      Supervisor.init(children, strategy: :one_for_all)
    end
    
  end
  
end
