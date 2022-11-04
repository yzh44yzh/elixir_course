defmodule NaiveServer do

  def start(port \\ 1234) do
    IO.puts("Start TCP Server at port #{port}")
    {:ok, listening_socket} = :gen_tcp.listen(port, [:binary, {:active, true}])
    IO.puts("Listening socket: #{inspect listening_socket}")
    start_acceptor(listening_socket)
  end

  def start_acceptor(listening_socket) do
    spawn(__MODULE__, :wait_for_client, [listening_socket])
  end

  def wait_for_client(listening_socket) do
    IO.puts("#{inspect self()} waits for client")
    {:ok, socket} = :gen_tcp.accept(listening_socket)
    IO.puts("#{inspect self()} got client connection #{inspect socket}")
    loop(listening_socket)
  end

  def loop(listening_socket) do
    IO.puts("#{inspect self()} is waiting for data from client")
    receive do
      {:tcp, socket, data} ->
        IO.puts("#{inspect self()} got data from client #{data}")
        :gen_tcp.send(socket, "ECHO #{data}")
        loop(listening_socket)
      {:tcp_closed, _socket} -> 
        IO.puts("#{inspect self()} connection closed")
        start_acceptor(listening_socket)
    end
  end
  
end
