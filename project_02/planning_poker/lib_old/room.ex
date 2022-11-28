defmodule PlanningPoker.Room do

  use GenServer

  require Logger

  defmodule State do
    defstruct [:room_name, :participants]
  end

  def start_link({room_name, process_name}) do
    GenServer.start_link(__MODULE__, room_name, name: process_name)
  end

  @impl true
  def init(room_name) do
    Logger.info("Start Room '#{room_name}'")
    state = %State{room_name: room_name, participants: []}
    {:ok, state}
  end

end
