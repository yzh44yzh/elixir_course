defmodule ChatServer do

  def start() do
    child_spec = [ChatServer.ClientSessionSup]
    Supervisor.start_link(child_spec, strategy: :one_for_one)
  end

  def start_session(client_id) do
    child_spec = {ChatServer.ClientSession, client_id}
    DynamicSupervisor.start_child(:session_manager, child_spec)
  end
    
  def stop_session(pid) do
    GenServer.call(pid, :stop_session)
  end


  defmodule ClientSessionSup do
    use DynamicSupervisor

    def start_link(_) do
      DynamicSupervisor.start_link(__MODULE__, :no_args, name: :session_manager)
    end

    def init(:no_args) do
      DynamicSupervisor.init(strategy: :one_for_one)
    end
  end

  defmodule ClientSession do
    use GenServer, restart: :transient

    def start_link(client_id) do
      GenServer.start_link(__MODULE__, [client_id])
    end

    @impl true
    def init([client_id]) do
      state = %{
        client_id: client_id
      }
      IO.puts("start ClientSession #{inspect self()} #{inspect state}")
      {:ok, state}
    end

    @impl true
    def handle_call(:stop_session, _from, state) do
      IO.puts("stop ClientSession #{inspect self()} #{inspect state}")
      {:stop, :normal, state}
    end

    def handle_call(unknown_msg, _from, state) do
      IO.puts("ERROR: unknown call #{inspect unknown_msg}")
      {:reply, {:error, :unknown_msg}, state}
    end
    
  end
  
end
  
