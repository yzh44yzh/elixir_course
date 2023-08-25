defmodule StringExample do

  def sample_words() do
    ["aa", "bbbb", "dddddd", "eee"]
  end
  
  @spec align([String.t]) :: [String.t]
  def align(words) do
    max_length =
      words
      |> Enum.map(&String.length/1)
      |> Enum.max

    Enum.map(words, fn w -> align_word(w, max_length) end)
  end

  @spec align_word(String.t, integer) :: String.t
  def align_word(word, length) do
    diff = length - String.length(word)
    left = div(diff, 2)
    right = diff - left
    make_padding(left) <> word <> make_padding(right)
  end

  def make_padding(0), do: ""
  
  def make_padding(length) when length > 0 do
    " " <> make_padding(length - 1)
  end

end

ExUnit.start()

defmodule StringExampleTest do
  use ExUnit.Case
  import StringExample

  test "make padding" do
    assert make_padding(0) == ""
    assert make_padding(1) == " "
    assert make_padding(2) == "  "
    assert make_padding(5) == "     "
  end
  
  test "align word" do
    assert " bob " == align_word("bob", 5)
    assert " bob  " == align_word("bob", 6)
    assert "  bob  " == align_word("bob", 7)
  end

  test "align" do
    assert ["  cat   ", " zebra  ", "elephant"] == align(~w'cat zebra elephant')
  end

end
