defmodule PathFinder2 do

  use GenServer

  @type city :: String.t
  @type distance :: integer
  @type route :: {[city], distance}

  @server_name __MODULE__
  @data_file "./data/cities.csv"
  

  # Module API
  
  def start() do
    GenServer.start(__MODULE__, @data_file, [name: @server_name])
  end


  @spec get_route(city, city) :: {:ok, route} | {:error, term}
  def get_route(from_city, to_city) do
    GenServer.call(@server_name, {:get_route, from_city, to_city})
  end

  def reload_data() do
    GenServer.cast(@server_name, :reload_data)
  end
  

  # GenServer callbacks

  @impl true
  def init(data_file) do
    state = %{data_file: data_file}
    {:ok, state, {:continue, :delayed_init}}
  end

  @impl true
  def handle_continue(:delayed_init, state) do
    case state do
      %{graph: graph} -> :digraph.delete(graph)
      _ -> :ok
    end

    %{data_file: data_file} = state
    new_state = init_state(data_file)
    {:noreply, new_state}
  end
  
  @impl true
  def handle_call({:get_route, from_city, to_city}, _from, state) do
    %{graph: graph, distances: distances} = state
    reply =
      case :digraph.get_short_path(graph, from_city, to_city) do
        false -> {:error, :no_route} 
        route ->
          distance = get_distance(distances, route)
          {:ok, route, distance}
      end
    {:reply, reply, state}
  end

  # catch all
  def handle_call(msg, from, state) do
    IO.puts("Server got unknow call #{inspect msg} from #{inspect from}")
    {:reply, {:error, :invalid_call}, state}
  end
  
  @impl true
  def handle_cast(:reload_data, state) do
    # NOTE: don't do `:digraph.delete(graph)` here
    {:noreply, state, {:continue, :delayed_init}}
  end

  # catch all
  def handle_cast(msg, state) do
    IO.puts("Server got unknow cast #{inspect msg}")
    {:noreply, state} 
  end

  @impl true
  def handle_info(msg, state) do
    IO.puts("Server got msg #{inspect msg}")
    {:noreply, state}
  end
  

  # Inner functions

  def init_state(file_name) do
    csv_data = load_csv_data(file_name)
    graph = :digraph.new([:cyclic])
    Enum.each(csv_data, fn(line) -> init_graph(graph, line) end)
    distances = init_distances(csv_data)
    %{
      graph: graph,
      distances: distances,
      data_file: file_name
    }
  end
  
  def load_csv_data(file_name) do
    File.read!(file_name)
    |> String.split()
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    [city1, city2, dist] = String.split(line, ",")
    {dist, ""} = Integer.parse(dist)
    {city1, city2, dist}
  end

  def init_graph(graph, {city1, city2, _dist}) do
    :digraph.add_vertex(graph, city1)
    :digraph.add_vertex(graph, city2)
    :digraph.add_edge(graph, city1, city2)
    :digraph.add_edge(graph, city2, city1)
  end

  def init_distances(csv_data) do
    Enum.reduce(csv_data, %{},
      fn ({city1, city2, dist}, acc) ->
        key = make_key(city1, city2)
        Map.put(acc, key, dist)
      end)
  end

  def make_key(city1, city2) do
    [city1, city2] |> Enum.sort() |> List.to_tuple()
  end
  
  def get_distance([], _distances), do: 0
  def get_distance(distancies, path) do
    [first | rest] = path
    Enum.reduce(
      rest,
      {first, 0},
      fn (city, {prev_city, dist}) ->
        key = make_key(city, prev_city)
        {city, dist + Map.fetch!(distancies, key)}
      end)
    |> elem(1)
  end
  
end
