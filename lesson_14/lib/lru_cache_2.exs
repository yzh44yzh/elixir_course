defmodule LRU_Cache do
  use GenServer

  ## Public API
  
  # NOTE: use start_link/1 to run under supervisor
  def start(options \\ %{}) do
    GenServer.start(__MODULE__, options, name: __MODULE__)
  end

  def stop() do
    GenServer.call(__MODULE__, :stop)
  end

  def get(key) do
    GenServer.call(__MODULE__, {:get, key})
  end

  def put(key, value) do
    GenServer.call(__MODULE__, {:put, key, value})
  end

  def delete(key) do
    GenServer.cast(__MODULE__, {:delete, key})
  end

  
  ## GenServer behavior

  @impl true
  def init(options) do
    IO.puts("init with options #{inspect options}")
    num_tables = Map.get(options, :num_tables, 5)
    key_lifetime = Map.get(options, :key_lifetime, 10000)
    state = %{
      num_tables: num_tables,
      key_lifetime: key_lifetime,
      tables: create_tables(num_tables)
    }
    IO.puts("state: #{inspect state}")
    {:ok, state}
  end

  @impl true
  def handle_call({:get, _key}, _from, state) do
    {:reply, :ok, state}
  end

  def handle_call({:put, _key, _value}, _from, state) do
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
  def handle_cast({:delete, _key}, state) do
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    IO.puts("ERROR: #{__MODULE__}.handle_cast got unknown message #{inspect msg}")
    {:noreply, state}
  end


  ## Private functions

  defp create_tables(num_tables) do
    Enum.map(1..num_tables, fn(id) ->
      table_name = String.to_atom("cache_table_#{id}")
      :ets.new(table_name, [:set, :private])
    end)
    |> :queue.from_list()
  end
  
end
