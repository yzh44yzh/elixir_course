defmodule Lesson_11.GS_1 do

  def start() do
    IO.puts("start Server")
    spawn(__MODULE__, :loop, [])
  end

  def loop() do
    IO.puts("Server enters #{inspect self()} loop")
    receive do
      msg ->
        IO.puts("Server #{inspect self()} got msg #{inspect msg}")
        loop()
    end
  end
  
end
