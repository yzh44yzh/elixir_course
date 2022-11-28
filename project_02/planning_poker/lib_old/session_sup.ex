defmodule PlanningPoker.SessionSup do

  use DynamicSupervisor

  require Logger

  @sup_name :session_sup
  @registry_name :session_registry

  def start_link(_) do
    Registry.start_link(keys: :unique, name: @registry_name)
    DynamicSupervisor.start_link(__MODULE__, :no_args, name: @sup_name)
  end

  def start_session(user) do
    process_name = {:via, Registry, {@registry_name, user.id}}
    spec = {PlanningPoker.Session, {user, process_name}}
    DynamicSupervisor.start_child(@sup_name, spec)
  end

  def find_session(user_id) do
    case Registry.lookup(@registry_name, user_id) do
      [{pid, _}] -> {:ok, pid}
      [] -> {:error, :not_found}
    end
  end

  @impl true
  def init(:no_args) do
    Logger.info("Start SessionSup")
    DynamicSupervisor.init(strategy: :one_for_one)
  end

end
