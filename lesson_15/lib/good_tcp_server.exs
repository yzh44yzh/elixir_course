defmodule GoodServer do

  def start(port \\ 1234) do
    IO.puts("Start TCP Server at port #{port}")
    {:ok, listening_socket} = :gen_tcp.listen(port, [:binary, {:active, true}])
    IO.puts("Listening socket: #{inspect listening_socket}")
    start_acceptor(listening_socket)
  end

  def start_acceptor(listening_socket) do
    spawn(__MODULE__, :wait_for_clients, [listening_socket])
  end
  
  def wait_for_clients(listening_socket) do
    IO.puts("Process #{inspect self()} is waiting for clients")
    {:ok, socket} = :gen_tcp.accept(listening_socket)
    IO.puts("Process #{inspect self()} got client connection #{inspect socket}")
    start_acceptor(listening_socket)
    state = %{listening_socket: listening_socket}
    loop(state)
  end

  def loop(state) do
    IO.puts("Process #{inspect self()} is waiting data from client")
    receive do
      {:tcp, socket, data} ->
        IO.puts("Process #{inspect self()} got data #{data}")
        :gen_tcp.send(socket, "ECHO: #{data}")
        loop(state)
      {:tcp_closed, _socket} ->
        IO.puts("Client close connection")
      msg ->
        IO.puts("Unknown msg #{inspect msg}")
    end
  end
  
end
