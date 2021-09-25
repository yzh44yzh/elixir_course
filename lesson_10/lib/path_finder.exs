defmodule PathFinder do

  @cities_file "./data/cities.csv"

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

  def init_graph(data) do
    graph = :digraph.new([:cyclic])
    Enum.reduce(data, graph, &add_item/2)
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

  def get_dist(graph, path) do
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
  
  def get_dist(graph, city1, city2) do
    edges1 = :digraph.edges(graph, city1)
    edges2 = :digraph.edges(graph, city2)
    [edge | _] = edges1 -- edges2
    {_, _, _, dist} = :digraph.edge(graph, edge)
    dist
  end
  
end
