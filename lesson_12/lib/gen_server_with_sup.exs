defmodule Lesson_12 do

  defmodule PathFinder do

    use GenServer

    @type city :: String.t
    @type distance :: integer
    @type route :: {[city], distance}

    @server_name __MODULE__
    @cities_file "../lesson_11/data/cities.csv"


    def start() do
      children = [
        {__MODULE__, [:no_args]}
      ]
      Supervisor.start_link(children, strategy: :one_for_all)
    end

    def start_link(_) do
      GenServer.start_link(__MODULE__, :no_args, [name: @server_name])
    end

    @spec get_route(city, city) :: {:ok, route} | {:error, term}
    def get_route(from_city, to_city) do
      GenServer.call(@server_name, {:get_route, from_city, to_city})
    end
    
    @impl true
    def init(:no_args) do
      state = %{}
      {:ok, state, {:continue, :delayed_init}}
    end

    @impl true
    def handle_continue(:delayed_init, state) do
      case state do
        %{graph: graph} -> :digraph.delete(graph)
        _ -> :ok
      end
      graph = :digraph.new([:cyclic])
      data = load_data(@cities_file)
      Enum.reduce(data, graph, &add_item/2)
      distancies = make_distancies_map(data)
      state = %{graph: graph, distancies: distancies}
      {:noreply, state}
    end
    
    @impl true
    def handle_call({:get_route, from_city, to_city}, _from, state) do
      %{graph: graph, distancies: distancies} = state
      reply =
        case :digraph.get_short_path(graph, from_city, to_city) do
          false -> {:error, :no_route} 
                   route ->
              distance = get_distance(distancies, route)
              {:ok, route, distance}
        end
      {:reply, reply, state}
    end

    def handle_call(unknown_msg, _from, state) do
      IO.puts("got unknown msg in handle_call: #{inspect unknown_msg}")
      {:reply, {:error, :invalid_call}, state}
    end
    
    @impl true
    def handle_info(msg, state) do
      IO.puts("got message #{inspect msg}")
      {:noreply, state}
    end
    

    # Inner functions

    defp load_data(path) do
      File.read!(path)
      |> String.split()
      |> Enum.map(&parse_line/1)
    end

    defp parse_line(line) do
      [city1, city2, dist] = String.split(line, ",")
      {dist, _} = Integer.parse(dist)
      {city1, city2, dist}
    end

    defp make_distancies_map(data) do
      Enum.reduce(data, %{},
        fn ({city1, city2, dist}, acc) ->
          key = make_key(city1, city2)
          Map.put(acc, key, dist)
        end)
    end

    defp make_key(city1, city2) do
      Enum.sort([city1, city2]) |> :erlang.list_to_tuple()
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

    defp get_distance(distancies, path) do
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

end
