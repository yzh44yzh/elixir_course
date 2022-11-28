defmodule Client do
  use GenServer

  def start() do
    start_link(:no_args)
  end

  def send_data(data) do
    GenServer.call(:client, {:send_data, data})
  end
  
  def start_link(_) do
    GenServer.start_link(__MODULE__, :no_args, name: :client)
  end

  @impl true
  def init(:no_args) do
    # host = {127, 0, 0, 1}
    host = 'localhost'
    port = 1234
    options = [
      :binary,
      {:active, true},
      {:packet, :raw}
      # {:packet, 2}
    ]
    IO.puts("TCP Client started")
    {:ok, socket} = :gen_tcp.connect(host, port, options)
    state = %{socket: socket}
    IO.puts("TCP Client connected to #{inspect host} #{port}")
    {:ok, state}
  end

  @impl true
  def handle_call({:send_data, data}, _from, state) do
    IO.puts("send to server: #{inspect data}")
    bin_data = :erlang.term_to_binary(data)
    size = byte_size(bin_data)
    header = <<size :: 16>>
    response = :gen_tcp.send(state.socket, header <> bin_data)
    # response = :gen_tcp.send(state.socket, bin_data)
    IO.puts("response #{inspect response}")
    {:reply, :ok, state}
  end

  # catch all
  def handle_call(msg, _from, state) do
    IO.puts("Client got unknown msg #{inspect msg}")
    {:reply, :error, state}
  end

  @impl true
  def handle_info({:tcp, _socket, data}, state) do
    <<size :: 16, rest :: binary>> = data
    data = :erlang.binary_to_term(rest)
    IO.puts("Client got data, size: #{size}, data: #{inspect data}")
    {:noreply, state}
  end
    
  # catch all
  def handle_info(msg, state) do
    IO.puts("Client got unknown info #{inspect msg}")
    {:noreply, state}
  end
end
