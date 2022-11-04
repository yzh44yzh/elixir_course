defmodule ChatServer do


  defmodule ClientSession do
    use GenServer, restart: :transient

    def start_link(client_id) do
      name = {:via, Registry, {:session_manager_registry, client_id}}
      GenServer.start_link(ClientSession, client_id, name: name)
    end

    def stop(pid) do
      GenServer.call(pid, :stop)
    end

    @impl true
    def init(client_id) do
      state = %{
        client_id: client_id
      }
      IO.puts("Start ClientSession pid:#{inspect self()}, state: #{inspect state}")
      {:ok, state}
    end

    @impl true
    def handle_call(:stop, _from, state) do
      %{client_id: client_id} = state
      IO.puts("Stop ClientSession pid:#{inspect self()}, client_id: #{client_id}")
      {:stop, :shutdown, state}
    end

    # catch all
    
  end

  defmodule SessionManager do
    use DynamicSupervisor

    # Module API
    def start_link(_) do
      Registry.start_link(keys: :unique, name: :session_manager_registry)
      DynamicSupervisor.start_link(__MODULE__, :no_args, name: :session_manager)
    end

    def start_session(client_id) do
      child_spec = {ClientSession, client_id}
      DynamicSupervisor.start_child(:session_manager, child_spec)
    end

    @spec stop_session(integer()) :: :ok | {:error, :not_found}
    def stop_session(client_id) do
      case Registry.lookup(:session_manager_registry, client_id) do
        [{pid, _}] ->
          ClientSession.stop(pid)
          :ok
        [] ->
          {:error, :not_found}
      end
    end

    # Callbacks
    @impl true
    def init(:no_args) do
      IO.puts("Start SessionManager")
      DynamicSupervisor.init(strategy: :one_for_one)
    end
  end

  def start() do
    SessionManager.start_link(:no_args)
  end
  
end
