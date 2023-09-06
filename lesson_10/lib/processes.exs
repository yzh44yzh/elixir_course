defmodule ProcessesExample do
  def run_processes(num) do
    IO.puts("__MODULE__:")
    IO.inspect(__MODULE__)
    for id <- 1..num, do: spawn(__MODULE__, :start, [id])
  end

  def start(id) do
    IO.puts("Process id:#{id} pid:#{inspect(self())} started")
    Process.sleep(Enum.random(10..500))
    IO.puts("Process id:#{id} pid:#{inspect(self())} stopped")
  end
end
