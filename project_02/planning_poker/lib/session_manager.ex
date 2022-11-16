defmodule PlanningPoker.SessionManager do

  use GenServer
  require Logger

  @name :session_manager

  ## Public API

  def start_link(_) do
    GenServer.start_link(__MODULE__, :no_args, name: @name)
  end

  
  ## GenServer Behaviour

  @impl true
  def init(_) do
    Logger.info("Start SessionManager")
    state = %{}
    {:ok, state}
  end

end
