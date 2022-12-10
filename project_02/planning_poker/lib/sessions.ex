defmodule PlanningPoker.Sessions do
  require Logger

  defmodule Session do
    use GenServer

    alias PlanningPoker.Rooms.{RoomManager, Room}
    alias PlanningPoker.Protocol

    defmodule State do
      defstruct [
        :session_id,
        :listening_socket,
        :socket,
        :user
      ]
    end

    def start_link({session_id, listening_socket}) do
      GenServer.start_link(__MODULE__, {session_id, listening_socket})
    end

    def send_event(session_pid, event) do
      Logger.info("Session.send_event #{inspect(session_pid)} #{inspect(event)}")
      GenServer.cast(session_pid, {:send_event, event})
    end

    @impl true
    def init({session_id, listening_socket}) do
      state = %State{
        session_id: session_id,
        listening_socket: listening_socket
      }

      Logger.info("Session #{inspect(self())} has started, #{inspect(state)}")
      {:ok, state, {:continue, :wait_for_client}}
    end

    @impl true
    def handle_continue(:wait_for_client, state) do
      IO.puts(
        "Session #{inspect(self())} #{state.session_id} is waiting for client #{inspect(state)}"
      )

      {:ok, socket} = :gen_tcp.accept(state.listening_socket)
      state = %State{state | socket: socket}
      IO.puts("Session #{state.session_id} got client connection #{inspect(socket)}")
      send(self(), :receive_data)
      {:noreply, state}
    end

    @impl true
    def handle_cast({:send_event, event}, state) do
      data = Protocol.serialize(event)
      Logger.info("send_event #{data} to #{inspect(state.socket)}")
      :gen_tcp.send(state.socket, data <> "\n")
      {:noreply, state}
    end

    @impl true
    def handle_info(:receive_data, state) do
      # IO.puts("Session #{inspect self()} #{state.session_id} is waiting for data #{inspect state}")
      case :gen_tcp.recv(state.socket, 0, 1000) do
        {:ok, data} ->
          IO.puts("Session #{state.session_id} has got data #{inspect(data)}")

          {response, state} =
            data
            |> String.trim()
            |> handle_request(state)

          :gen_tcp.send(state.socket, response <> "\n")
          send(self(), :receive_data)
          {:noreply, state}

        {:error, :timeout} ->
          send(self(), :receive_data)
          {:noreply, state}

        {:error, error} ->
          IO.puts("Session #{state.session_id} has got #{inspect(error)}")
          :gen_tcp.close(state.socket)
          state = on_client_disconnect(state)
          {:noreply, state, {:continue, :wait_for_client}}
      end
    end

    # catch all
    def handle_info(msg, state) do
      Logger.error("Session #{inspect(self())} unknown info #{inspect(msg)}")
      {:noreply, state}
    end

    defp handle_request(request, state) do
      case Protocol.deserialize(request) do
        {:error, error} ->
          {Protocol.serialize({:error, error}), state}

        event ->
          {result, state} = handle_event(event, state)
          {Protocol.serialize(result), state}
      end
    end

    defp handle_event({:login, name}, state) do
      alias PlanningPoker.UsersDatabase

      case UsersDatabase.get_by_name(name) do
        {:ok, user} ->
          Logger.info("auth as #{inspect(user)}")
          Registry.register(:sessions_registry, user.id, user)
          state = %State{state | user: user}
          {:ok, state}

        {:error, :not_found} ->
          {{:error, :invalid_auth}, state}
      end
    end

    defp handle_event({:join_room, _room_name}, %State{user: nil} = state) do
      {{:error, :forbidden}, state}
    end

    defp handle_event({:join_room, room_name}, state) do
      response =
        case RoomManager.find_room(room_name) do
          {:ok, room_pid} -> Room.join(room_pid, state.user)
          error -> error
        end

      {response, state}
    end

    # catch all
    defp handle_event(event, state) do
      Logger.error("Unknown event #{inspect(event)}")
      # status 500
      result = {:error, :unknown_error}
      {result, state}
    end

    defp on_client_disconnect(state) do
      Registry.unregister(:sessions_registry, state.user.id)
      {:ok, room_pid} = RoomManager.find_room("Room 1")
      Room.leave(room_pid, state.user)
      # RoomManager.leave_all_rooms(state.user)
      %State{state | user: nil}
    end
  end

  defmodule SessionManager do
    use GenServer

    defmodule State do
      defstruct [
        :port,
        :pool_size,
        :listening_socket
      ]
    end

    def start_link({port, pool_size}) do
      GenServer.start_link(__MODULE__, {port, pool_size})
    end

    @impl true
    def init({port, pool_size}) do
      state = %State{port: port, pool_size: pool_size}
      Logger.info("SessionManager has started, #{inspect(state)}")
      {:ok, state, {:continue, :delayed_init}}
    end

    @impl true
    def handle_continue(:delayed_init, state) do
      options = [
        :binary,
        {:active, false},
        {:packet, :line},
        {:reuseaddr, true}
      ]

      {:ok, listening_socket} = :gen_tcp.listen(state.port, options)
      Logger.info("SessionManager is listening socket #{inspect(listening_socket)}")

      Registry.start_link(keys: :unique, name: :sessions_registry)

      1..state.pool_size
      |> Enum.each(fn session_id ->
        PlanningPoker.Sessions.SessionSup.start_acceptor(session_id, listening_socket)
      end)

      state = %State{state | listening_socket: listening_socket}
      {:noreply, state}
    end
  end

  defmodule SessionSup do
    use DynamicSupervisor

    @name :session_sup

    def start_link(_) do
      DynamicSupervisor.start_link(__MODULE__, :no_args, name: @name)
    end

    def start_acceptor(session_id, listening_socket) do
      spec = {Session, {session_id, listening_socket}}
      DynamicSupervisor.start_child(@name, spec)
    end

    @impl true
    def init(_) do
      Logger.info("SessionSup has started")
      DynamicSupervisor.init(strategy: :one_for_one)
    end
  end
end
