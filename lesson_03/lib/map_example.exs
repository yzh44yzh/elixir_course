defmodule MapExample do
  @moduledoc """
  Посчитать, сколько раз встречается каждое слово в строке.
  """

  def test_string() do
    """
    Elixir in Action is a tutorial book that aims to bring developers
    new to Elixir and Erlang to the point where they can develop complex systems on their own.
    """
  end

  @spec count_words(String.t) :: %{String.t => integer}
  def count_words(string) do
    string
    |> String.split()
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

ExUnit.start()

defmodule MapExampleTest do
  use ExUnit.Case
  import MapExample

  test "count words" do
    assert %{"Hello" => 1, "world" => 1} == count_words("Hello world")
    assert %{"Bip" => 2, "bop" => 5, "bip" => 2, "bam" => 1} ==
             count_words(" Bip bop bip bop bop Bip bop bip bop bam ")
  end

end
