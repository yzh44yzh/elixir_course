defmodule PathFinder do

  use GenServer

  @type city :: String.t
  @type distance :: integer
  @type route :: {[city], distance}

  ## Public API

  def start_link({name, data_file}) do
    GenServer.start_link(__MODULE__, [data_file], name: name)
  end

  def get_route(name, city1, city2) do
    GenServer.call(name, {:get_route, city1, city2})
  end

  def reload_data(name) do
    GenServer.cast(name, :reload_data)
  end

  
  ## behaviour GenServer

  @impl true
  def init([data_file]) do
    IO.puts("init")
    state = %{data_file: data_file}
    {:ok, state, {:continue, :delayed_init}}
  end

  @impl true
  def handle_continue(:delayed_init, %{:data_file => data_file} = state) do
    IO.puts("handle_continue")
    case state do
      %{:graph => graph} -> :digraph.delete(graph)
      _ -> :ok
    end
    graph = :digraph.new()
    data = load_data!(data_file)
    init_graph(graph, data)
    dist_map = init_distancies(data)
    state = %{
      graph: graph,
      dist: dist_map,
      data_file: data_file
    }
    {:noreply, state}
  end

  @impl true
  def handle_call(
    {:get_route, city1, city2},
    _from,
    %{:graph => graph, :dist => dist} = state
  ) do
    reply = case :digraph.get_short_path(graph, city1, city2) do
              false -> {:error, :no_route}
              route ->
                total_dist = get_total_dist(route, dist)
                {:ok, route, total_dist}
            end
    {:reply, reply, state}
  end

  def handle_call(unknown_message, _from, state) do
    IO.puts("ERROR: PathFinder.call got unknown message #{unknown_message}")
    {:reply, {:error, :unknown_message}, state}
  end

  @impl true
  def handle_cast(:reload_data, state) do
    IO.puts("reload_data")
    {:noreply, state, {:continue, :delayed_init}}
  end
  
  def handle_cast(unknown_message, state) do
    IO.puts("ERROR: PathFinder.cast got unknown message #{unknown_message}")
    {:noreply, state}
  end
  
  @impl true
  def handle_info(message, state) do
    IO.puts("handle_info #{inspect message}")
    {:noreply, state}
  end

  def terminate(state) do
    {:noreply, state}
  end

  

  ## private functions

  defp load_data!(data_file) do
    File.read!(data_file)
    |> String.split()
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    [city1, city2, dist] = String.split(line, ",")
    {dist, _} = Integer.parse(dist)
    {city1, city2, dist}
  end

  defp init_graph(graph, data) do
    Enum.each(data,
      fn({city1, city2, _dist}) ->
        :digraph.add_vertex(graph, city1)
        :digraph.add_vertex(graph, city2)
        :digraph.add_edge(graph, city1, city2)
        :digraph.add_edge(graph, city2, city1)
      end)
  end

  defp init_distancies(data) do
    Enum.reduce(data, %{},
      fn({city1, city2, dist}, acc) ->
        key = make_key(city1, city2)
        Map.put(acc, key, dist)
        end)
  end

  defp make_key(city1, city2), do: Enum.sort([city1, city2]) |> :erlang.list_to_tuple()

  defp get_total_dist(route, dist_map) do
    [first | rest] = route
    acc = {first, 0}
    Enum.reduce(rest, acc,
      fn(curr_city, {prev_city, total_dist}) ->
        key = make_key(curr_city, prev_city)
        dist = Map.fetch!(dist_map, key)
        {curr_city, total_dist + dist}
      end)
    |> elem(1)
  end

end


