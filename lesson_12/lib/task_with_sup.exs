defmodule Lesson_12 do

  defmodule FindSourcesTask do

    def start_without_sup(path) do
      Task.async(__MODULE__, :find_elixir_sources, [path])
    end

    def start_with_sup(path) do
      {:ok, sup_pid} = Task.Supervisor.start_link()
      Task.Supervisor.async(sup_pid, __MODULE__, :find_elixir_sources, [path])
    end

    # start in sup tree
    def start_link(path) do
      Task.start_link(__MODULE__, :find_elixir_sources, [path])
    end

    def get_result(task) do
      Task.await(task)
    end
    
    def find_elixir_sources(path) do
      # find lib -name *.ex -o -name *.exs
      {response, 0} = System.cmd("find", [path, "-name", "*.exs", "-o", "-name", "*.ex"])
      String.split(response, "\n")
      |> Enum.filter(fn(line) -> line != "" end)
    end
    
  end

end
