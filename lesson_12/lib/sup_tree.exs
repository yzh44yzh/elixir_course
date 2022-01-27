defmodule App do

  def start() do
    IO.puts("App.start #{inspect self()}")
    App.RootSup.start_link(:no_args)
  end
  
  defmodule RootSup do
    use Supervisor

    def start_link(_) do
      IO.puts("RootSup.start_link #{inspect self()}")
      Supervisor.start_link(__MODULE__, :no_args)
    end
    
    @impl true
    def init(_) do
      IO.puts("RootSup.init #{inspect self()}")
      child_spec = [
        App.AgentSup,
        App.PathFinderSup
      ]
      Supervisor.init(child_spec, strategy: :one_for_all)
    end
  end

  defmodule AgentSup do
    use Supervisor

    def start_link(_) do
      IO.puts("AgentSup.start_link #{inspect self()}")
      Supervisor.start_link(__MODULE__, :no_args)
    end
    
    @impl true
    def init(_) do
      IO.puts("AgentSup.init #{inspect self()}")
      state_a = [
        { 0, 11, "Node-1"},
        {12, 23, "Node-2"},
        {24, 35, "Node-3"},
        {36, 47, "Node-4"}
      ]
      state_b = [
        { 0, 7, "NodeA"},
        {8, 15, "NodeB"}
      ]
      child_spec = [
        %{
          id: :agent_a,
          start: {Lesson_12.ShardingAgent, :start_link, [{:agent_a, state_a}]}
        },
        %{
          id: :agent_b,
          start: {Lesson_12.ShardingAgent, :start_link, [{:agent_b, state_b}]}
        }
      ]      
      Supervisor.init(child_spec, strategy: :one_for_all)
    end
  end

  defmodule PathFinderSup do
    use Supervisor

    def start_link(_) do
      IO.puts("PathFinderSup.start_link #{inspect self()}")
      Supervisor.start_link(__MODULE__, :no_args)
    end
    
    @impl true
    def init(_) do
      IO.puts("PathFinderSup.init #{inspect self()}")
      child_spec = [
        %{
          id: :pf_ru,
          start: {PathFinder, :start_link, [{:pf_ru, "./cities.csv"}]}
        },
        %{
          id: :pf_eu,
          start: {PathFinder, :start_link, [{:pf_eu, "./cities_eu.csv"}]}
        }
      ]
      Supervisor.init(child_spec, strategy: :one_for_all)
    end
  end

  
end
