defmodule HOF do

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
    splitter = fn({:user, _id, _name, user_age} = user, {older, younger}) ->
      if user_age > age do
        {[user | older], younger}
      else
        {older, [user | younger]}
      end
    end
    Enum.reduce(users, {[], []}, splitter)
  end


  def get_longest_name_user(users) do
    [first | rest] = users
    finder = fn(user, longest_name_user) ->
      {:user, _, name, _} = user
      {:user, _, longest_name, _} = longest_name_user
      if String.length(longest_name) < String.length(name), do: user, else: longest_name_user
    end
    Enum.reduce(rest, first, finder)
  end


  def get_oldest_user(users) do
    finder = fn(user, oldest_user) ->
      {:user, _, _, age} = user
      {:user, _, _, oldest_age} = oldest_user
      if oldest_age < age, do: user, else: oldest_user
    end
    Enum.reduce(users, finder)
  end

end
