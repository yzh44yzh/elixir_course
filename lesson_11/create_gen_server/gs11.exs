defmodule Lesson_11.GS_11 do

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
    ref = Process.monitor(pid)
    send(pid, {:call, ref, self(), msg})
    receive do
      {:reply, ^ref, reply} ->
        Process.demonitor(ref, [:flush])
        reply
      {:DOWN, ^ref, :process, ^pid, reason} ->
        {:error, reason}
    after @timeout -> :noreply
    end
  end    
  
  def loop(state) do
    server_name = "[Server 6] #{inspect self()}"
    IO.puts("#{server_name} enters loop")
    receive do
      {:call, ref, from, msg} ->
        {reply, new_state} = handle_call(msg, state)
        send(from, {:reply, ref, reply})
        __MODULE__.loop(new_state)
      :stop ->
        IO.puts("#{server_name} stops now")
      msg ->
        IO.puts("ERROR: #{server_name} got unknown msg #{inspect msg}")
        __MODULE__.loop(state)
    end
  end

  def handle_call({:add, item}, state) do
    new_state = [item | state]
    {:ok, new_state}
  end

  def handle_call({:remove, item}, state) do
    new_state = List.delete(state, item)
    {:ok, new_state}
  end

  def handle_call({:check, item}, state) do
    result = Enum.member?(state, item)
    {result, state}
  end

  def handle_call(:show, state) do
    10 / 0  # crash server process 
    {state, state}
  end
  
end
