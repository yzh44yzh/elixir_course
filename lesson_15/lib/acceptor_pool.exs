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

    def start_link(args) do
      GenServer.start_link(__MODULE__, args)
    end

    @impl true
    def init({id, listening_socket}) do
      state = %{
        id: id,
        listening_socket: listening_socket,
        socket: nil
      }
      IO.puts("Start #{acceptor_info(state)} with state #{inspect state}")
      {:ok, state, {:continue, :wait_for_client}}
    end

    @impl true
    def handle_continue(:wait_for_client, state) do
      info = acceptor_info(state)
      IO.puts("#{info} is waiting for client")
      {:ok, socket} = :gen_tcp.accept(state.listening_socket)
      IO.puts("#{info} has got client connection #{inspect socket}")
      state = %{state | socket: socket}
      send(self(), :receive_data)
      {:noreply, state}
    end

    @impl true
    def handle_info(:receive_data, state) do
      info = acceptor_info(state)
      IO.puts("#{info} is waiting for data")
      case :gen_tcp.recv(state.socket, 2, 2000) do
        {:ok, header} ->
          <<header_size :: 16>> = header
          {:ok, data} = :gen_tcp.recv(state.socket, header_size)
          data = :erlang.binary_to_term(data)
          IO.puts("#{info} has got data #{inspect data}")

          response = %{
            susccess: true,
            request: data
          }
          response_bin = :erlang.term_to_binary(response)
          size = byte_size(response_bin)
          response_bin = <<size :: 16>> <> response_bin
          
          :gen_tcp.send(state.socket, response_bin)
          send(self(), :receive_data)
          {:noreply, state}
        {:error, :timeout} ->
          IO.puts("timeout")
          send(self(), :receive_data)
          {:noreply, state}
        {:error, error} ->
          IO.puts("#{info} has got #{inspect error}")
          :gen_tcp.close(state.socket)
          {:noreply, state, {:continue, :wait_for_client}}
      end          
    end

    def handle_info({:tcp_closed, _socket}, state) do
      %{socket: socket} = state
      IO.puts("#{acceptor_info(state)} client has closed the connection")
      :gen_tcp.close(socket)
      {:noreply, state, {:continue, :wait_for_client}}
    end

    def handle_info({:tcp, _socket, "quit\r\n"}, state) do
      %{socket: socket} = state
      IO.puts("#{acceptor_info(state)} closes connection")
      :gen_tcp.close(socket)
      {:noreply, state, {:continue, :wait_for_client}}
    end

    def handle_info({:tcp, _socket, data}, state) do
      %{socket: socket} = state
      IO.puts("#{acceptor_info(state)} got data from client #{inspect data}")
      :gen_tcp.send(socket, "ECHO #{data}")
      {:noreply, state}
    end
    
    # catch all
    def handle_info(msg, state) do
      IO.puts("#{acceptor_info(state)} got unknown msg #{inspect msg}")
      {:noreply, state}
    end

    defp acceptor_info(state) do
      "Acceptor #{state.id} #{inspect self()}"
    end
    
  end
  
  defmodule AcceptorSup do
    use DynamicSupervisor

    @name :acceptor_sup

    def start_link(:no_args) do
      DynamicSupervisor.start_link(__MODULE__, :no_args, name: @name)
    end

    def start_acceptor(id, listening_socket) do
      spec = {Acceptor, {id, listening_socket}}
      DynamicSupervisor.start_child(@name, spec)
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
        # {:active, true},
        {:active, false},
        # {:packet, 2},
        {:packet, :raw},
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
      
      Enum.each(1..pool_size, fn(id) -> 
        AcceptorSup.start_acceptor(id, listening_socket)
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
      Supervisor.init(spec, strategy: :rest_for_one)
    end
  end

  def start() do
    IO.puts("Server starts")
    RootSup.start_link(:no_args)
  end
  
end
