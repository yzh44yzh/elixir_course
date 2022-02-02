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
      AgentSup,
      PathFinderSup
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
    state_a = Application.get_env(:my_cool_app, :agent_a_state)
    state_b = Application.get_env(:my_cool_app, :agent_b_state)
    child_spec = [
      %{
        id: :agent_a,
        start: {ShardingAgent, :start_link, [{:agent_a, state_a}]}
      },
      %{
        id: :agent_b,
        start: {ShardingAgent, :start_link, [{:agent_b, state_b}]}
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
    priv_dir = Application.app_dir(:my_cool_app, "priv")
    cities_ru = Path.join(priv_dir, Application.get_env(:my_cool_app, :cities_ru))
    cities_eu = Path.join(priv_dir, Application.get_env(:my_cool_app, :cities_eu))
    child_spec = [
      %{
        id: :pf_ru,
        start: {PathFinder, :start_link, [{:pf_ru, cities_ru}]}
      },
      %{
        id: :pf_eu,
        start: {PathFinder, :start_link, [{:pf_eu, cities_eu}]}
      }
    ]
    Supervisor.init(child_spec, strategy: :one_for_all)
  end
end

