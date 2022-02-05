defmodule MyCoolProject do

  defmodule App do
    use Application
    require Logger

    @impl true
    def start(_start_type, _args) do
      Logger.info("start #{__MODULE__}")
      Supervisor.start_link(MyCoolProject.RootSup, :no_args)
    end
    
  end

  defmodule RootSup do
    use Supervisor

    @impl true
    def init(_) do
      state_a = Application.get_env(:my_cool_project, :agent_a_state)
      state_b = Application.get_env(:my_cool_project, :agent_b_state)
      child_spec = [
      %{
        id: :agent_a,
        start: {MyCoolProject.Worker, :start_link, [{:agent_a, state_a}]}
      },
      %{
        id: :agent_b,
        start: {MyCoolProject.Worker, :start_link, [{:agent_b, state_b}]}
      }      ]
      Supervisor.init(child_spec, strategy: :one_for_one)
    end
  end

  defmodule Worker do
    use Agent, restart: :permanent
    require Logger

    def start_link({agent_name, state}) do
      Logger.info("start agent #{agent_name} with state #{inspect state}")
      Agent.start_link(fn() -> state end, name: agent_name)
    end    
  end

end
