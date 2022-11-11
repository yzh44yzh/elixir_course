defmodule Server do

  @moduledoc """
  sup tree:
  - RootSup
    - Listener
    - AcceptorSup
      - Acceptor 1
      - Acceptor 2
  """

  defmodule Acceptor do
    use GenServer

    def start_link(listening_socket) do
      GenServer.start_link(__MODULE__, listening_socket)
    end

    @impl true
    def init(listening_socket) do
      state = %{
        listening_socket: listening_socket,
        socket: nil
      }
      IO.puts("Acceptor starts with state #{inspect state}")
      {:ok, state, {:continue, :wait_for_client}}
    end

    @impl true
    def handle_continue(:wait_for_client, state) do
      %{listening_socket: listening_socket} = state
      IO.puts("Acceptor #{inspect self()} waits for client")
      {:ok, socket} = :gen_tcp.accept(listening_socket)
      IO.puts("Acceptor #{inspect self()} got client connection #{inspect socket}")
      state = %{state | socket: socket}
      {:noreply, state}
    end

    @impl true
    def handle_info({:tcp_closed, _socket}, state) do
      %{socket: socket} = state
      IO.puts("Acceptor #{inspect self()} client has closed the connection")
      :gen_tcp.close(socket)
      {:noreply, state, {:continue, :wait_for_client}}
    end

    def handle_info({:tcp, _socket, "quit\r\n"}, state) do
      %{socket: socket} = state
      IO.puts("Acceptor #{inspect self()} closes connection")
      :gen_tcp.close(socket)
      {:noreply, state, {:continue, :wait_for_client}}
    end

    def handle_info({:tcp, _socket, data}, state) do
      %{socket: socket} = state
      IO.puts("Acceptor #{inspect self()} got data from client #{inspect data}")
      :gen_tcp.send(socket, "ECHO #{data}")
      {:noreply, state}
    end
    
    # catch all
    def handle_info(msg, state) do
      IO.puts("unknown info #{inspect msg}")
      {:noreply, state}
    end
    
  end
  
  defmodule AcceptorSup do
    use DynamicSupervisor

    def start_link(:no_args) do
      DynamicSupervisor.start_link(__MODULE__, :no_args, name: __MODULE__)
    end

    def start_acceptor(listening_socket) do
      spec = {Acceptor, listening_socket}
      DynamicSupervisor.start_child(__MODULE__, spec)
    end

    @impl true
    def init(:no_args) do
      IO.puts("AcceptorSup starts")
      DynamicSupervisor.init(strategy: :one_for_one)
    end
  end

  defmodule Listener do
    use GenServer

    def start_link(options) do
      GenServer.start_link(__MODULE__, options)
    end

    @impl true
    def init(options) do
      port = Keyword.get(options, :port, 1234)
      pool_size = Keyword.get(options, :pool_size, 10)

      options = [
        :binary,
        {:active, true},
        {:reuseaddr, true}
      ]
      {:ok, listening_socket} = :gen_tcp.listen(port, options)

      state = %{
        port: port,
        pool_size: pool_size,
        listening_socket: listening_socket
      }
      IO.puts("Listener starts with state #{inspect state}")
      {:ok, state, {:continue, :delayed_init}}
    end

    @impl true
    def handle_continue(:delayed_init, state) do
      %{listening_socket: listening_socket, pool_size: pool_size} = state
      
      Enum.each(1..pool_size, fn(_) -> 
        AcceptorSup.start_acceptor(listening_socket)
      end)
      
      {:noreply, state}
    end
  end

  defmodule RootSup do
    use Supervisor

    def start_link(:no_args) do
      Supervisor.start_link(__MODULE__, :no_args)
    end

    @impl true
    def init(:no_args) do
      IO.puts("RootSup starts")
      spec = [
        {AcceptorSup, :no_args}, # should be started before Listener
        {Listener, [port: 1234, pool_size: 5]}
      ]
      Supervisor.init(spec, strategy: :one_for_one)
    end
  end

  def start() do
    IO.puts("Server starts")
    RootSup.start_link(:no_args)
  end
  
end
