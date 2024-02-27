defmodule StringExample do

  def example() do
    ["a", "bb", "hello", "world", "some-long-word", "short-word"]
  end

  @doc """
  |  a  |
  |  bb |
  | ccc |
  | dddd|
  |eeeee|
  """
  def align_words(words) do
    max_length = find_max_length(words)

    Enum.map(words, fn word -> align_word(word, max_length) end)
  end

  def find_max_length(words) do
    find_max_length(words, 0)
  end

  defp find_max_length([], current_max), do: current_max

  defp find_max_length([word | words], current_max) do
    current_max = max(String.length(word), current_max)
    find_max_length(words, current_max)
  end

  # TODO protect from infinitive recursion
  # length should not be less than word length
  def align_word(word, length) do
    spaces = length - String.length(word)
    left_spaces = div(spaces, 2)
    right_spaces = spaces - left_spaces

    word
    |> add_spaces(:right, right_spaces)
    |> add_spaces(:left, left_spaces)
  end

  # TODO protect from infinitive recursion
  # spaces should not be less than 0
  def add_spaces(str, _, 0), do: str
  def add_spaces(str, :left, spaces), do: add_spaces(" " <> str, :left, spaces - 1)
  def add_spaces(str, :right, spaces), do: add_spaces(str <> " ", :right, spaces - 1)
end

ExUnit.start()

defmodule StringExampleTest do
  use ExUnit.Case
  import StringExample

  test "add_spaces" do
    assert add_spaces("aa", :left, 0) == "aa"
    assert add_spaces("aa", :right, 0) == "aa"
    assert add_spaces("aa", :left, 2) == "  aa"
    assert add_spaces("aa", :right, 2) == "aa  "
  end

  test "align_word" do
    assert align_word("aa", 2) == "aa"
    assert align_word("aa", 3) == "aa "
    assert align_word("aa", 4) == " aa "
    assert align_word("aa", 5) == " aa  "
    assert align_word("aa", 6) == "  aa  "
  end

  test "find max length" do
    assert find_max_length(example()) == 14
    assert find_max_length([]) == 0
    assert find_max_length(["a"]) == 1
    assert find_max_length(["a", "ddd"]) == 3
  end

  test "align_words" do
    assert align_words([]) == []
    assert align_words(["aa"]) == ["aa"]
    assert align_words(["aa", "bbb"]) == ["aa ", "bbb"]
    assert align_words(["aa", "bbbb"]) == [" aa ", "bbbb"]
    assert align_words(["aa", "bbbb", "cccccc"]) == ["  aa  ", " bbbb ", "cccccc"]
  end
end
