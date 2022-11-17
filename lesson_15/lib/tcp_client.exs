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
    IO.puts("response #{inspect response}")
    {:reply, :ok, state}
  end

  # catch all
  def handle_call(msg, _from, state) do
    IO.puts("Client got unknown msg #{inspect msg}")
    {:reply, :error, state}
  end
end
