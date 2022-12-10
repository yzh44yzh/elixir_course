defmodule PlanningPoker.Protocol do
  require Logger

  def deserialize("login " <> name), do: {:login, name}
  def deserialize("join " <> room_name), do: {:join_room, room_name}
  def deserialize("topic " <> description), do: {:topic, description}

  def deserialize("vote " <> points) do
    # TODO validation
    {points, _} = Integer.parse(points)
    {:vote, points}
  end

  def deserialize("show"), do: :show
  def deserialize("quit"), do: :quit

  def deserialize(msg) do
    Logger.warning("Protocol: unknown data #{msg}")
    {:error, :unknown_msg}
  end

  def serialize({:logged_in, user_name, role}),
    # TODO use
    do: "logged in, name: #{user_name} role: #{role}"

  def serialize({:joined, user, room_name}),
    do: "#{user.name} #{user.role} has joined to the room #{room_name}"

  def serialize({:topic, description}), do: "Topic: #{description}"
  def serialize({:voted, user}), do: "#{user.name} has voted"
  def serialize({:show, results}), do: "Vote results: #{inspect(results)}"

  def serialize({:leaved, user, room_name}),
    do: "#{user.name} has left the room #{room_name}"

  def serialize(:ok), do: "OK"
  def serialize({:error, error}), do: "ERROR: #{inspect(error)}"
end
