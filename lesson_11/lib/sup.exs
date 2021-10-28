defmodule Lesson_11 do

  defmodule MyApp do

    def start_sup_tree() do
      children = [
        Lesson_11.RootSup
      ]
      Supervisor.start_link(children, strategy: :one_for_one) # TODO more options
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
        {Lesson_11.AgentSup, [:no_args]},
        {Lesson_11.PathFinder, [:no_args]}
      ]
      Supervisor.init(children, strategy: :one_for_all) # TODO more options
    end
    
  end

  defmodule AgentSup do
    use Supervisor

    def start_link(args) do
      Supervisor.start_link(__MODULE__, args, name: __MODULE__)
    end

    @impl true
    def init(_args) do
      # TODO get num agents from args
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
          start: {Lesson_11.ShardingAgent, :start_link, [{:agent_a, state_1}]}
        },
        %{
          id: :agent_b,
          start: {Lesson_11.ShardingAgent, :start_link, [{:agent_b, state_2}]}
        }
      ]
      Supervisor.init(children, strategy: :one_for_all)
    end
    
  end
  
end
