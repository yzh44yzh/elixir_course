defmodule Lesson_11.GS_8 do

  @timeout 5000
  
  def start() do
    IO.puts("start Server")
    initial_state = []
    spawn(__MODULE__, :loop, [initial_state])
  end

  def add(pid, item) do
    call(pid, {:add, item})
  end

  def remove(pid, item) do
    call(pid, {:remove, item})
  end

  def check(pid, item) do
    call(pid, {:check, item})
  end

  def show(pid) do
    call(pid, :show)
  end

  def stop(pid) do
    send(pid, :stop)
  end

  def call(pid, msg) do
    send(pid, {self(), msg})
    receive do
      {:reply, reply} -> reply
    after @timeout -> :noreply
    end
  end    
  
  def loop(state) do
    server_name = "[Server 6] #{inspect self()}"
    IO.puts("#{server_name} enters loop")
    receive do
      {from, {:add, item}} ->
        new_state = [item | state]
        send(from, {:reply, :ok})
        __MODULE__.loop(new_state)
      {from, {:remove, item}} ->
        new_state = List.delete(state, item)
        send(from, {:reply, :ok})
        __MODULE__.loop(new_state)
      {from, {:check, item}} ->
        res = Enum.member?(state, item)
        send(from, {:reply, res})
        __MODULE__.loop(state)
      {from, :show} ->
        send(from, {:reply, state})
        __MODULE__.loop(state)
      :stop ->
        IO.puts("#{server_name} stops now")
      msg ->
        IO.puts("ERROR: #{server_name} got unknown msg #{inspect msg}")
        __MODULE__.loop(state)
    end
  end
  
end
