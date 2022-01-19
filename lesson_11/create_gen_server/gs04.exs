defmodule Lesson_11.GS_4 do

  def start() do
    IO.puts("start Server 4")
    initial_state = []
    spawn(__MODULE__, :loop, [initial_state])
  end

  def loop(state) do
    server_name = "[Server 4] #{inspect self()}"
    IO.puts("#{server_name} enters loop")
    receive do
      {:add, item} ->
        new_state = [item | state]
        __MODULE__.loop(new_state)
      {:remove, item} ->
        new_state = List.delete(state, item)
        __MODULE__.loop(new_state)
      {:check, item} ->
        res = Enum.member?(state, item)
        IO.puts("check #{res}")
        __MODULE__.loop(state)
      :show ->
        IO.puts("current state is #{inspect state}")
        __MODULE__.loop(state)
      :stop ->
        IO.puts("#{server_name} stops now")
      msg ->
        IO.puts("ERROR: #{server_name} got unknown msg #{inspect msg}")
        __MODULE__.loop(state)
    end
  end
  
end
