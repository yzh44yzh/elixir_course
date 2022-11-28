defmodule PlanningPoker.RoomSup do

  use DynamicSupervisor

  require Logger

  @sup_name :room_sup
  @registry_name :room_registry

  def start_link(_) do
    Registry.start_link(keys: :unique, name: @registry_name)
    DynamicSupervisor.start_link(__MODULE__, :no_args, name: @sup_name)
  end

  def start_room(room_name) do
    process_name = {:via, Registry, {@registry_name, room_name}}
    spec = {PlanningPoker.Room, {room_name, process_name}}
    DynamicSupervisor.start_child(@sup_name, spec)
  end

  def find_room(room_name) do
    case Registry.lookup(@registry_name, room_name) do
      [{pid, _}] -> {:ok, pid}
      [] -> {:error, :not_found}
    end
  end

  @impl true
  def init(:no_args) do
    Logger.info("Start RoomSup")
    DynamicSupervisor.init(strategy: :one_for_one)
  end

end
