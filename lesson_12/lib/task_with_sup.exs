defmodule Lesson_12 do

  defmodule FindSourcesTask do

    def start(path) do
      {:ok, sup_pid} = Task.Supervisor.start_link()
      Task.Supervisor.async(sup_pid, __MODULE__, :find_elixir_sources, [path])
    end

    def start_without_sup(path) do
      Task.async(__MODULE__, :find_elixir_sources, [path])
    end

    def get_result(task) do
      Task.await(task)
    end
    
    def find_elixir_sources(path) do
      # find lib -name *.ex -o -name *.exs
      {response, 0} = System.cmd("find", [path, "-name", "*.exs", "-o", "-name", "*.exs"])
      String.split(response, "\n")
      |> Enum.filter(fn(line) -> line != "" end)
    end
    
  end

end
