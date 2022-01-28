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
    key_lifetime = Map.get(options, :key_lifetime, 60000) # miliseconds

    rotate_time = div(key_lifetime, num_tables)
    :erlang.send_after(rotate_time, self(), :rotate)
    
    state = %{
      rotate_time: rotate_time,
      tables: create_tables(num_tables)
    }
    IO.puts("state: #{inspect state}")
    {:ok, state}
  end

  @impl true
  def handle_call({:get, key}, _from, %{tables: tables} = state) do
    reply = case lookup(key, tables) do
              {:ok, value} ->
                [top_table | _] = tables
                :ets.insert(top_table, {key, value})
                {:ok, value}
              :not_found -> {:error, :not_found}
            end
    {:reply, reply, state}
  end

  def handle_call({:put, key, value}, _from, %{tables: tables} = state) do
    [top_table | _] = tables
    :ets.insert(top_table, {key, value})
    size = :ets.info(top_table, :size)
    IO.puts("put #{inspect key} into #{inspect top_table}, now it has #{size} items")
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
  def handle_cast({:delete, key}, %{tables: tables} = state) do
    Enum.each(tables, fn(table) -> :ets.delete(table, key) end)
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    IO.puts("ERROR: #{__MODULE__}.handle_cast got unknown message #{inspect msg}")
    {:noreply, state}
  end

  @impl true
  def handle_info(:rotate, %{tables: tables, rotate_time: rotate_time} = state) do
    tables = rotate_tables(tables)
    state = %{state | tables: tables}
    :erlang.send_after(rotate_time, self(), :rotate)
    {:noreply, state}
  end
  
  def handle_info(msg, state) do
    IO.puts("ERROR: #{__MODULE__}.handle_info got unknown message #{inspect msg}")
    {:noreply, state}
  end

  ## Private functions

  defp create_tables(num_tables) do
    Enum.map(1..num_tables, fn(id) ->
      table_name = String.to_atom("cache_table_#{id}")
      :ets.new(table_name, [:set, :private])
    end)
  end

  defp rotate_tables(tables) do
    IO.puts("rotate_tables #{inspect tables}")
    [last_table | rest] = Enum.reverse(tables)
    table_name = :ets.info(last_table, :name)
    IO.puts("drop #{table_name}")
    :ets.delete(last_table)
    new_table = :ets.new(table_name, [:set, :private])
    [new_table | Enum.reverse(rest)]
  end

  defp lookup(key, tables) do
    Enum.reduce(tables, :not_found,
      fn
        (_, {:ok, _} = acc) -> acc
        (table, :not_found) ->
          case :ets.lookup(table, key) do
            [] -> :not_found
            [{^key, value}] -> {:ok, value}
          end
      end)
  end

end
