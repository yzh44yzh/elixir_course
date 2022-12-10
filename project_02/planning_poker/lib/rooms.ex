defmodule PlanningPoker.Rooms do
  require Logger

  defmodule Room do
    use GenServer

    alias PlanningPoker.Model.Room

    def start_link({room_name, process_name}) do
      GenServer.start_link(__MODULE__, room_name, name: process_name)
    end

    def join(room_pid, user) do
      GenServer.call(room_pid, {:join, user})
    end

    def leave(room_pid, user) do
      GenServer.call(room_pid, {:leave, user})
    end

    def broadcast(room_pid, event) do
      GenServer.call(room_pid, {:broadcast, event})
    end

    @impl true
    def init(room_name) do
      state = %Room{
        name: room_name,
        participants: []
      }

      Logger.info("#{inspect(state)} has started")
      {:ok, state}
    end

    @impl true
    def handle_call({:join, user}, _from, state) do
      if user in state.participants do
        {:reply, {:error, :already_joined}, state}
      else
        participants = [user | state.participants]
        state = %Room{state | participants: participants}
        Logger.info("user has joined the room #{inspect(state)}")
        state = do_broadcast({:joined, user, state.name}, state)
        {:reply, :ok, state}
      end
    end

    def handle_call({:leave, user}, _from, state) do
      participants = List.delete(state.participants, user)
      state = %Room{state | participants: participants}
      Logger.info("user has left the room #{inspect(state)}")
      state = do_broadcast({:leaved, user, state.name}, state)
      {:reply, :ok, state}
    end

    def handle_call({:broadcast, event}, _from, state) do
      state = do_broadcast(event, state)
      {:reply, :ok, state}
    end

    # catch all
    def handle_call(msg, _from, state) do
      Logger.error("Room unknown call #{inspect(msg)}")
      {:reply, :error, state}
    end

    defp do_broadcast(event, state) do
      Logger.info("Room.do_broadcast #{inspect(event)}")

      Enum.each(
        state.participants,
        fn user ->
          case Registry.lookup(:sessions_registry, user.id) do
            [] -> Logger.error("Session for user #{user.id} is not found")
            [{session_pid, _}] -> PlanningPoker.Sessions.Session.send_event(session_pid, event)
          end
        end
      )

      state
    end
  end

  defmodule Sup do
    use DynamicSupervisor

    @sup_name :room_sup
    # TODO leaked into RoomManager
    @registry_name :room_registry

    def start_link(_) do
      Registry.start_link(keys: :unique, name: @registry_name)
      DynamicSupervisor.start_link(__MODULE__, :no_args, name: @sup_name)
    end

    def start_room(room_name) do
      process_name = {:via, Registry, {@registry_name, room_name}}
      spec = {Room, {room_name, process_name}}
      DynamicSupervisor.start_child(@sup_name, spec)
    end

    @impl true
    def init(_) do
      Logger.info("#{@sup_name} has started")
      DynamicSupervisor.init(strategy: :one_for_one)
    end
  end

  defmodule RoomManager do
    use GenServer

    defmodule State do
      defstruct [:rooms]
    end

    @process_name :room_manager

    def start_link(_) do
      GenServer.start_link(__MODULE__, :no_args, name: @process_name)
    end

    def start_room(room_name) do
      GenServer.call(@process_name, {:start_room, room_name})
    end

    def find_room(room_name) do
      case Registry.lookup(:room_registry, room_name) do
        [{room_pid, _}] -> {:ok, room_pid}
        [] -> {:error, :not_found}
      end
    end

    @impl true
    def init(_) do
      state = %State{
        rooms: []
      }

      Logger.info("RoomManager has started, #{inspect(state)}")
      {:ok, state}
    end

    @impl true
    def handle_call({:start_room, room_name}, _from, %State{rooms: rooms} = state) do
      {:ok, _} = Sup.start_room(room_name)
      state = %State{state | rooms: [room_name | rooms]}
      Logger.info("RoomManager has started room #{inspect(state)}")
      {:reply, :ok, state}
    end

    # TODO catch all
  end
end
