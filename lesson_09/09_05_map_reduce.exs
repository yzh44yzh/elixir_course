defmodule Lesson_09.Task_05_Map_Reduce do

  # Mapper
  defmodule Worker do

    def start(parent, file) do
      IO.puts("start worker #{inspect self()} with file '#{file}'")
      num_words = process(file)
      send(parent, {:result, self(), num_words})
    end

    defp process(file) do
      {:ok, content} = File.read(file)
      String.split(content) |> length()
    end
    
  end

  # Reducer
  def start() do
    start([
      "./09_01_processes.md",
      "./09_02_mailbox.md",
      "./09_03_link.md",
      "./09_04_monitor.md"
    ])
  end
  
  def start(files) do
    IO.puts("start #{inspect files}")
    workers = for file <- files do
      spawn(Worker, :start, [self(), file])
    end
    wait_for_results(workers, 0)
  end

  defp wait_for_results([], acc) do
    IO.puts("DONE")
    acc
  end
  
  defp wait_for_results(workers, acc) do
    receive do
      {:result, pid, num_words} ->
        IO.puts("got result #{num_words} from #{inspect pid}")
        wait_for_results(List.delete(workers, pid), acc + num_words)
      msg ->
        IO.puts("got unknown msg #{inspect msg}")
        wait_for_results(workers, acc)
    after 5000 ->
        IO.puts("got no results")
    end
  end
  
end
