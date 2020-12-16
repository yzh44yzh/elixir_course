defmodule Lesson_03.Task_03_07_String do

  def align(words) do
    max_length = words
    |> Enum.map(&String.length/1)
    |> Enum.max
    Enum.map(words, fn w -> align_word(w, max_length) end)
    |> Enum.join("\n")
  end

  def align_word(word, to_length) do
    length = String.length(word)
    left = div(to_length - length, 2)
    String.pad_leading(word, left + length)
    |> String.pad_trailing(to_length)
  end

end

ExUnit.start()

defmodule Task_03_07_Test do
  use ExUnit.Case
  import Lesson_03.Task_03_07_String

  test "align word" do
    assert " bob " == align_word("bob", 5)
    assert " bob  " == align_word("bob", 6)
    assert "  bob  " == align_word("bob", 7)
  end

  test "align" do
    assert "  cat   \n zebra  \nelephant" == align(~w'cat zebra elephant')
  end

end