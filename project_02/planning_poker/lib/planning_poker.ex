defmodule PlanningPoker do

  use Application

  require Logger

  @impl true
  def start(_start_type, _args) do
    Logger.info("Start PlanningPoker")
    PlanningPoker.RootSup.start_link(:no_args)
  end

  defmodule RootSup do
    use Supervisor

    def start_link(_) do
      Supervisor.start_link(__MODULE__, :no_args)
    end

    @impl true
    def init(_) do
      child_spec = [
        {PlanningPoker.RoomSup, :no_args},
        {PlanningPoker.RoomManager, :no_args},
        {PlanningPoker.SessionSup, :no_args},
        {PlanningPoker.SessionManager, :no_args}
      ]
      Supervisor.init(child_spec, strategy: :one_for_all)
    end
  end

  
end
