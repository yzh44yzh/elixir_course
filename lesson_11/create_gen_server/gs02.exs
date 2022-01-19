defmodule Lesson_11.GS_2 do

  def start() do
    IO.puts("start Server")
    spawn(__MODULE__, :loop, [])
  end

  def loop() do
    server_name = "Server #{inspect self()}"
    IO.puts("#{server_name} enters loop")
    receive do
      :stop ->
        IO.puts("#{server_name} stops now")
      msg ->
        IO.puts("ERROR: #{server_name} got unknown msg #{inspect msg}")
        loop()
    end
  end
  
end
