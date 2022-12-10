defmodule PlanningPoker do
  use Application

  require Logger

  def start(_start_type, _args) do
    Logger.info("Start PlanningPoker")
    res = PlanningPoker.RootSup.start_link(:no_args)
    PlanningPoker.Rooms.RoomManager.start_room("Room 1")
    res
  end

  defmodule RootSup do
    use Supervisor

    def start_link(_) do
      Supervisor.start_link(__MODULE__, :no_args)
    end

    @impl true
    def init(_) do
      port = 1234
      pool_size = 5

      spec = [
        {PlanningPoker.Rooms.Sup, :no_args},
        {PlanningPoker.Rooms.RoomManager, :no_args},
        {PlanningPoker.Sessions.SessionSup, :no_args},
        {PlanningPoker.Sessions.SessionManager, {port, pool_size}}
      ]

      Supervisor.init(spec, strategy: :rest_for_one)
    end
  end
end
