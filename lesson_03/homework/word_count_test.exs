ExUnit.start()

defmodule WordCountTest do
  use ExUnit.Case
  doctest WordCount
  import WordCount

  test "count" do
    assert count("Hello") == {1, 1, 5}
    assert count("Hello there") == {1, 2, 11}
    assert count("Hello there \n hello world") == {2, 4, 25}
  end

  test "count with cyrillic symbols" do
    assert count("Привет") == {1, 1, 6}
  end

  test "process_file" do
    assert process_file("data.txt") == {9, 331, 2031}
  end

end