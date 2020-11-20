defmodule Lesson_03.Task_03_06_Map do
  @moduledoc """
  Посчитать, сколько раз встречается каждое слово в строке
  """

  def test_string() do
    """
    Elixir in Action is a tutorial book that aims to bring developers
    new to Elixir and Erlang to the point where they can develop complex systems on their own.
    """
  end

  def count_words(string) do
    String.split(string)
    |> count_words(%{})
  end

  defp count_words([], acc), do: acc
  defp count_words([word | words], acc) do
    acc = case Map.fetch(acc, word) do
      {:ok, count} -> Map.put(acc, word, count + 1)
      :error -> Map.put(acc, word, 1)
    end
    count_words(words, acc)
  end

end