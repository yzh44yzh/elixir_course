defmodule CoolTcpServer do

  @moduledoc """
  RootSup
  - Listener
  - AcceptorSup 
    - Acceptor
  """

  defmodule Listener do
    use GenServer

    def start_link(options) do
      GenServer.start_link(__MODULE__, options)
    end

    @impl true
    def init(options) do
      state = %{
        port: Map.get(options, :port, 1234),
        pool_size: Map.get(options, :pool_size, 50)
      }

      socket_options = [
        :binary,
        {:active, true},
        # {:active, false},
        # {:packet, :line},
        {:reuseaddr, true}
      ]
      {:ok, listening_socket} = :gen_tcp.listen(state.port, socket_options)
      state = Map.put(state, :listening_socker, listening_socket)
      IO.puts("Start Listener with state #{inspect state}")
      1..state.pool_size |> Enum.each(
        fn(id) -> CoolTcpServer.AcceptorSup.start_acceptor(id, listening_socket) end
      )
      {:ok, state}
    end
    
  end

  defmodule Acceptor do
    use GenServer

    def start_link(args) do
      GenServer.start_link(__MODULE__, args)
    end

    @impl true
    def init({id, listening_socket}) do
      state = %{
        id: id,
        listening_socket: listening_socket
      }
      IO.puts("Start Acceptor #{id} with state #{inspect state}")
      {:ok, state, {:continue, :wait_for_client}}
    end

    @impl true
    def handle_continue(:wait_for_client, state) do
      IO.puts("Start Acceptor #{state.id} is waiting for client")
      {:ok, socket} = :gen_tcp.accept(state.listening_socket)
      state = Map.put(state, :socket, socket)
      IO.puts("Start Acceptor #{state.id} got client connetion #{inspect socket}")
      # {:noreply, state}
      {:noreply, state, {:continue, :receive_data}}
    end

    def handle_continue(:receive_data, state) do
      IO.puts("Start Acceptor #{state.id} is waiting for data")
      case :gen_tcp.recv(state.socket, 0, 2000) do
        {:ok, data} ->
          IO.puts("Start Acceptor #{state.id} has got data #{inspect data}")
          :gen_tcp.send(state.socket, "ECHO: #{data}")
          {:noreply, state, {:continue, :receive_data}}
        {:error, :timeout} ->
          {:noreply, state, {:continue, :receive_data}}
        {:error, error} ->
          IO.puts("Start Acceptor #{state.id} has got #{inspect error}")
          :gen_tcp.close(state.socket)
          {:noreply, state, {:continue, :wait_for_client}}
      end          
    end

    # catch all
    def handle_continue(msg, state) do
      IO.puts("Acceptor #{state.id} got unknown continue #{inspect msg}")
      {:noreply, state}
    end

    @impl true
    def handle_info({:tcp, socket, "hello" <> _}, state) do
      :gen_tcp.send(socket, "Nice to meet you\n")
      {:noreply, state}
    end

    def handle_info({:tcp, socket, "quit" <> _}, state) do
      :gen_tcp.close(socket)
      {:noreply, state, {:continue, :wait_for_client}}
    end

    def handle_info({:tcp_closed, _socket}, state) do
      {:noreply, state, {:continue, :wait_for_client}}
    end

    def handle_info({:tcp, socket, msg}, state) do
      IO.puts("Acceptor #{state.id} got msg #{inspect msg}")
      :gen_tcp.send(socket, "ECHO: #{msg}")
      {:noreply, state}
    end
      
    # catch all
    def handle_info(msg, state) do
      IO.puts("Acceptor #{state.id} got unknown msg #{inspect msg}")
      {:noreply, state}
    end
    
  end

  defmodule AcceptorSup do
    use DynamicSupervisor

    @name :acceptor_sup

    def start_link(_) do
      DynamicSupervisor.start_link(__MODULE__, :no_args, name: @name)
    end

    def start_acceptor(id, listening_socket) do
      spec = {Acceptor, {id, listening_socket}}
      DynamicSupervisor.start_child(@name, spec)
    end

    @impl true
    def init(_) do
      DynamicSupervisor.init(strategy: :one_for_one)
    end
  end
  
  defmodule RootSup do
    use Supervisor

    def start_link(_) do
      Supervisor.start_link(__MODULE__, :no_args)
    end

    @impl true
    def init(_) do
      options = %{
        port: 1234,
        pool_size: 5
      }
      spec = [
        {AcceptorSup, :no_args},
        {Listener, options}
      ]
      Supervisor.init(spec, strategy: :rest_for_one)
    end
  end

  def start() do
    IO.puts("CoolTcpServer.start")
    RootSup.start_link(:no_args)
  end
  
end
