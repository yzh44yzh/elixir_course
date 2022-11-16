defmodule PlanningPoker.Server.Protocol do

  def serialize({:logged_in, user_name, role}), do: "logged in, name: #{user_name} role: #{role}"
  def serialize({:joined, user_name}), do: "#{user_name} has joined the room"
  def serialize({:set_topic, description}), do: "new topic #{description}"
  def serialize({:voted, user_name}), do: "#{user_name} voted"
  def serialize({:voting_finished, results}), do: "voting finished #{inspect results}"
  def serialize({:leaved, user_name}), do: "#{user_name} has left the room"
  def serialize(:ok), do: "OK"
  def serialize(:unknown_command), do: "Error: unknown command"
  
  def deserialize("login " <> user_name), do: {:login, user_name}
  def deserialize("join " <> room), do: {:join, room}
  def deserialize("topic " <> description), do: {:topic, description}
  def deserialize("vote " <> points), do: {:vote, points} # TODO validation
  def deserialize("show" <> _), do: :show
  def deserialize("quit" <> _), do: :quit
  def deserialize(_), do: :unknown_command

end
