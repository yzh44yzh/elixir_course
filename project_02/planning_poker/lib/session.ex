defmodule PlanningPoker.Session do

  use GenServer

  require Logger

  defmodule State do
    defstruct [:user, :socket]
  end

  def start_link({user, process_name}) do
    GenServer.start_link(__MODULE__, user, name: process_name)
  end

  @impl true
  def init(user) do
    Logger.info("Start Session for #{inspect user}")
    state = %State{user: user}
    {:ok, state}
  end

end
