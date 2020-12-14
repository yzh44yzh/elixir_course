defmodule Lesson_04.Task_04_05_Accumulator do

  def test_data() do
    # {:user, id, name, age}
    [
      {:user, 1, "Bob", 23},
      {:user, 2, "Helen", 20},
      {:user, 3, "Bill", 15},
      {:user, 4, "Kate", 14}
    ]
  end


  def filter_adults(users), do: filter_adults(users, [])


  defp filter_adults([], acc), do: Enum.reverse(acc)

  defp filter_adults([user | users], acc) do
    {:user, _, _, age} = user
    if age > 16 do
      filter_adults(users, [user | acc])
    else
      filter_adults(users, acc)
    end
  end


  def get_id_name(users), do: get_id_name(users, [])

  defp get_id_name([], acc), do: Enum.reverse(acc)

  defp get_id_name([user | users], acc) do
    {:user, id, name, _} = user
    get_id_name(users, [{id, name} | acc])
  end


  def split_teens_and_adults(users), do: split_teens_and_adults(users, {[], []})

  defp split_teens_and_adults([], {teens, adults}) do
    {Enum.reverse(teens), Enum.reverse(adults)}
  end

  defp split_teens_and_adults([user | users], {teens, adults}) do
    {:user, _, _, age} = user
    if age > 16 do
      split_teens_and_adults(users, {teens, [user | adults]})
    else
      split_teens_and_adults(users, {[user | teens], adults})
    end
  end

end

ExUnit.start()

defmodule Task_04_Test do
  use ExUnit.Case
  import Lesson_04.Task_04_05_Accumulator

  test "align word" do
    # assert " bob " == align_word("bob", 5)
  end

end