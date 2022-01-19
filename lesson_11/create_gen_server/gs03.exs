defmodule Lesson_11.GS_3 do

  def start() do
    IO.puts("start Server")
    initial_state = []
    spawn(__MODULE__, :loop, [initial_state])
  end

  def loop(state) do
    server_name = "Server #{inspect self()}"
    IO.puts("#{server_name} enters loop")
    receive do
      {:add, item} ->
        new_state = [item | state]
        loop(new_state)
      {:remove, item} ->
        new_state = List.delete(state, item)
        loop(new_state)
      {:check, item} ->
        res = Enum.member?(state, item)
        IO.puts("check #{res}")
        loop(state)
      :show ->
        IO.puts("current state is #{inspect state}")
        loop(state)
      :stop ->
        IO.puts("#{server_name} stops now")
      msg ->
        IO.puts("ERROR: #{server_name} got unknown msg #{inspect msg}")
        loop(state)
    end
  end
  
end
