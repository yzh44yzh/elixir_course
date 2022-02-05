defmodule LRU_Cache do
  use GenServer

  ## Public API
  
  # NOTE: use start_link/1 to run under supervisor
  def start() do
    GenServer.start(__MODULE__, :no_args, name: __MODULE__)
  end

  def stop() do
    GenServer.call(__MODULE__, :stop)
  end

  def get(key) do
    case :ets.lookup(__MODULE__, key) do
      [{^key, value, created, lifetime_ms}] ->
        if valid?(created, lifetime_ms) do
          GenServer.cast(__MODULE__, {:update, key}) 
          {:ok, value}
        else
          delete(key) # cast does not block client
          {:error, :not_found}
        end
      [] -> {:error, :not_found}
    end
  end

  defp valid?(created, lifetime_ms) do
    now = :os.system_time(:millisecond)
    IO.puts("valid? #{created} #{lifetime_ms} #{now}")
    now < created + lifetime_ms
  end

  def put(key, value, lifetime_sec \\ 10) do
    GenServer.call(__MODULE__, {:put, key, value, lifetime_sec})
  end

  def delete(key) do
    GenServer.cast(__MODULE__, {:delete, key})
  end

  
  ## GenServer behavior

  @impl true
  def init(:no_args) do
    state = %{}
    :ets.new(__MODULE__, [:named_table, :set, :protected])
    {:ok, state}
  end

  @impl true
  def handle_call({:put, key, value, lifetime_sec}, _from, state) do
    created = :os.system_time(:millisecond)
    lifetime_ms = lifetime_sec * 1000
    :ets.insert(__MODULE__, {key, value, created, lifetime_ms})
    {:reply, :ok, state}
  end

  def handle_call(:stop, _from, state) do
    IO.puts("Stop #{__MODULE__}")
    {:stop, :shutdown, state}
  end
  
  def handle_call(msg, _from, state) do
    IO.puts("ERROR: #{__MODULE__}.handle_call got unknown message #{inspect msg}")
    {:reply, {:error, :unknown_message}, state}
  end

  @impl true
  def handle_cast({:update, key}, state) do
    IO.puts("update #{key}")
    case :ets.lookup(__MODULE__, key) do
      [{^key, value, _, lifetime_ms}] ->
        created = :os.system_time(:millisecond)
        :ets.insert(__MODULE__, {key, value, created, lifetime_ms})
      [] -> :ok
    end
    {:noreply, state}
  end
  
  def handle_cast({:delete, key}, state) do
    IO.puts("delete #{key}")
    :ets.delete(__MODULE__, key)
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    IO.puts("ERROR: #{__MODULE__}.handle_cast got unknown message #{inspect msg}")
    {:noreply, state}
  end
  
end
