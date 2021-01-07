defmodule Lesson_05.Task_05_02_HOF do

  def test_data() do
    # {:user, id, name, age}
    [
      {:user, 1, "Bob", 23},
      {:user, 2, "Helen", 20},
      {:user, 3, "Bill", 15},
      {:user, 4, "Kate", 14}
    ]
  end


  def get_average_age(users) do
    collect_stat = fn({:user, _id, _name, age}, {total_users, total_age}) ->
      {total_users + 1, total_age + age}
    end
    {total_users, total_age} = Enum.reduce(users, {0, 0}, collect_stat)
    total_age / total_users
  end


  def split_by_age(users) do
    splitter = fn ({:user, _id, _name, age} = user, {adults, children}) ->
      if age > 16 do
        {[user | adults], children}
      else
        {adults, [user | children]}
      end
    end
    Enum.reduce(users, {[], []}, splitter)
  end


  def split_by_age(users, age) do
    splitter = fn ({:user, _id, _name, user_age} = user, {older, younger}) ->
      if user_age > age do
        {[user | older], younger}
      else
        {older, [user | younger]}
      end
    end
    Enum.reduce(users, {[], []}, splitter)
  end

end