defmodule PlanningPoker.SessionSup do

  use DynamicSupervisor

  require Logger

  @name :session_sup

  def start_link(_) do
    DynamicSupervisor.start_link(__MODULE__, :no_args, name: @name)
  end

  @impl true
  def init(:no_args) do
    Logger.info("Start SessionSup")
    DynamicSupervisor.init(strategy: :one_for_one)
  end

end
