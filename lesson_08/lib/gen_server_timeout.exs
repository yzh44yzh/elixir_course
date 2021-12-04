defmodule Lesson_07.Task_02_GenServerTimeout do

  def start_server() do
    {:ok, _} = GenServer.start_link(MyServer, [], name: MyServer)
  end
  
  def normal_request() do
    GenServer.call(MyServer, :normal_request)
  end

  def long_request_with_resque() do
    try do
      GenServer.call(MyServer, :long_request)
    rescue
      error -> IO.puts("Got error #{inspect error}")
    end
  end

  def long_request_with_catch() do
    try do
      GenServer.call(MyServer, :long_request)
    catch
      err_type, error -> IO.puts("Got error #{err_type} #{inspect error}")
    end
  end

end


defmodule MyServer do
  use GenServer

  @impl true
  def init(_) do
    {:ok, %{}}
  end

  @impl true
  def handle_call(:normal_request, _from, state) do
    response = {:ok, 42}
    {:reply, response, state}
  end

  def handle_call(:long_request, _from, state) do
    :timer.sleep(6000)
    response = {:ok, 42}
    {:reply, response, state}
  end

  
end
