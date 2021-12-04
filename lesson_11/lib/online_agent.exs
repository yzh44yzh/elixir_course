defmodule Lesson_10.Task_02_Online do

  def start() do
    state = []
    Agent.start(fn () -> state end, [name: :online_users])
    :ok
  end

  def add_user(username) do
    shard = :erlang.phash2(username, 48)
    {:ok, node} = Lesson_10.Task_02_Sharding.find_node(shard)
    Agent.update(:online_users, fn(users) -> [{username, shard, node} | users] end)
  end

  def get_users() do
    Agent.get(:online_users, fn(users) -> users end)
  end    

       
end
