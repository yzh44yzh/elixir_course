defmodule Lesson11 do

  defmodule Session do
    @type t :: %Session{
      username: String.t(),
      num_shard: integer(),
      node_name: String.t()
    }
    
    defstruct [:username, :num_shard, :node_name]
  end

  defmodule SessionManager do
    @type state() :: [Session.t()]

    @spec add_session(pid(), String.t()) :: :ok
    def add_session(agent_pid, username) do
      shard = 1
      node = "Node-1"
      session = %Session{username: username, num_shard: shard, node_name: node}
      Agent.update(agent_pid, fn(state) -> [session | state] end)
      :ok
    end

    @spec get_sessions(pid()) :: [Session.t()]
    def get_sessions(agent_pid) do
      Agent.get(agent_pid, fn(state) -> state end)
    end

    def get_session_by_name(agent_pid, name) do
      Agent.get(agent_pid, fn(state) -> find_session(state, name) end)
    end

    @spec start() :: pid()
    def start() do
      state = []
      Agent.start(fn() -> state end)
    end

    def stop(pid) do
      Agent.stop(pid)
    end

    defp find_session(sessions, name) do
      Enum.find(sessions, fn(session) -> session.username == name end)
    end
    
  end
  
end
