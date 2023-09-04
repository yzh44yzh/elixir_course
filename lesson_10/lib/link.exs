defmodule LinkExample do
  def run_and_exit(num) do
    for id <- 1..num, do: spawn(__MODULE__, :start_and_exit, [id])
    # for id <- 1..num, do: spawn_link(__MODULE__, :start_and_exit, [id])
  end

  def start_and_exit(id) do
    IO.puts("Process id:#{id} pid:#{inspect(self())} started")

    if id == 3 do
      IO.puts("Process id:#{id} exits")
      exit(:some_reason)
    end

    Process.sleep(Enum.random(10..500))
    IO.puts("Process id:#{id} pid:#{inspect(self())} stopped")
  end
end
