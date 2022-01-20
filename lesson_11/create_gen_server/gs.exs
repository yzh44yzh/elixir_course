defmodule GS do

  def start() do
    IO.puts("start server")
    state = %{}
    spawn(__MODULE__, :loop, [state])
  end

  def add(pid, k, v) do
    call(pid, {:add, k, v})
  end

  def remove(pid, k) do
    call(pid, {:remove, k})
  end

  def find(pid, k) do
    call(pid, {:find, k})
  end

  def get_state(pid) do
    call(pid, :get_state)
  end

  defp call(server_pid, msg) do
    ref = Process.monitor(server_pid)
    send(server_pid, {:call, self(), ref, msg})
    receive do
      {:reply, ^ref, reply} ->
        Process.demonitor(ref, [:flush])
        reply
      {:DOWN, ^ref, :process, ^server_pid, reason} ->
        {:server_error, reason}
    after 5000 -> :no_reply
    end
  end

  def loop(state) do
    server_name = "Server #{inspect self()}"
    IO.puts("#{server_name} enters loop with state #{inspect state}")
    receive do
      {:call, from, ref, msg} ->
        {reply, new_state} = handle_call(msg, state)
        send(from, {:reply, ref, reply})
        __MODULE__.loop(new_state)
      :stop ->
        IO.puts("#{server_name} stops")
      msg ->
        IO.puts("#{server_name} got unknown message #{inspect msg}")
        __MODULE__.loop(state)
    end
  end

  def handle_call({:add, k, v}, state) do
    new_state = Map.put(state, k, v)
    reply = :ok
    {reply, new_state}
  end

  def handle_call({:remove, k}, state) do
    new_state = Map.drop(state, [k])
    reply = :ok
    10 / 0
    {reply, new_state}
  end

  def handle_call({:find, k}, state) do
    reply = Map.fetch(state, k)
    {reply, state}
  end

  def handle_call(:get_state, state) do
    reply = state
    {reply, state}
  end
  
end
