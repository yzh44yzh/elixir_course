defmodule PathFinder do

  use GenServer

  @type city :: String.t
  @type distance :: integer
  @type route :: {[city], distance}

  @server_name __MODULE__
  @cities_file "./data/cities.csv"

  # Module API
  
  def start() do
    GenServer.start(__MODULE__, :no_args, [name: @server_name])
  end


  @spec get_route(city, city) :: route
  def get_route(from_city, to_city) do
    GenServer.call(@server_name, {:get_route, from_city, to_city})
  end


  # GenServer callbacks

  @impl true
  def init(:no_args) do
    graph = :digraph.new([:cyclic])
    load_data() |> Enum.reduce(graph, &add_item/2)
    state = %{graph: graph}
    {:ok, state}
  end

  @impl true
  def handle_call({:get_route, from_city, to_city}, _from, %{:graph => graph} = state) do
    route = :digraph.get_short_path(graph, from_city, to_city)
    dist = get_dist(graph, route)
    reply = {route, dist}
    {:reply, reply, state}
  end


  # Inner functions
  def load_data() do
    load_data(@cities_file)
  end

  def load_data(path) do
    File.read!(path)
    |> String.split()
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    [city1, city2, dist] = String.split(line, ",")
    {dist, _} = Integer.parse(dist)
    {city1, city2, dist}
  end
  
  defp add_item({city1, city2, dist} = item, graph) do
    v1 = :digraph.add_vertex(graph, city1) # non-functional, (mutates ETS) 
    v2 = :digraph.add_vertex(graph, city2)
    :digraph.add_edge(graph, v1, v2, dist)
    res = :digraph.add_edge(graph, v2, v1, dist) # imitate non-directed graph with two directions
    case res do
      {:error, e} -> raise "error adding item #{inspect item}, #{inspect e}"
      _ -> :ok
    end
    graph
  end

  defp get_dist(_graph, []), do: 0
  defp get_dist(graph, path) do
    [first | rest] = path
    Enum.reduce(
      rest,
      {first, 0},
      fn (city, {prev_city, dist}) ->
        city_dist = get_dist(graph, prev_city, city)
        {city, dist + city_dist}
      end)
    |> elem(1)
  end
  
  defp get_dist(graph, city1, city2) do
    edges1 = :digraph.edges(graph, city1)
    edges2 = :digraph.edges(graph, city2)
    case edges1 -- edges2 do
      [edge | _] -> 
        {_, _, _, dist} = :digraph.edge(graph, edge)
        dist
      [] -> 0
    end
  end
  
end
