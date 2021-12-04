defmodule CustomExceptionsTest do
  use ExUnit.Case
  import Homework

  test "get_from_list!" do
    list = [1, 2, 3, 4]
    assert get_from_list!(list, 0) == 1
    assert get_from_list!(list, 1) == 2
    assert get_from_list!(list, 2) == 3
    assert get_from_list!(list, 3) == 4

    assert_raise IndexOutOfBoundsError, "index 4 is out of bounds [0-4)",
    fn ->
      get_from_list!(list, 4)
    end

    assert_raise IndexOutOfBoundsError, "index -1 is out of bounds [0-4)",
    fn ->
      get_from_list!(list, -1)
    end

    assert_raise IndexOutOfBoundsError, "index 42 is out of bounds [0-4)",
    fn ->
      get_from_list!(list, 42)
    end
  end

  test "get_from_list" do
    list = [3, 4, 5, 6, 7, 8]
    assert get_from_list(list, 0) == {:ok, 3}
    assert get_from_list(list, 1) == {:ok, 4}
    assert get_from_list(list, 2) == {:ok, 5}
    assert get_from_list(list, 3) == {:ok, 6}
    assert get_from_list(list, 4) == {:ok, 7}
    assert get_from_list(list, 5) == {:ok, 8}

    assert get_from_list(list, 6) == {:error, "index 6 is out of bounds [0-6)"}
    assert get_from_list(list, -5) == {:error, "index -5 is out of bounds [0-6)"}
    assert get_from_list(list, 42) == {:error, "index 42 is out of bounds [0-6)"}
  end

  test "get_name_from_list! sample" do
    list = ["cat", "dog", "fish"]
    assert get_many_from_list!(list, [0, 0, 2, 2]) == ["cat", "cat", "fish", "fish"]
  end
  
  test "get_many_from_list!" do
    list = [10, 20, 30, 40, 50]
    assert get_many_from_list!(list, [0, 1]) == [10, 20]
    assert get_many_from_list!(list, [0, 2, 4]) == [10, 30, 50]
    assert get_many_from_list!(list, [1, 3]) == [20, 40]
    assert get_many_from_list!(list, [0, 1, 2, 3, 4]) == list
    assert get_many_from_list!(list, [1, 1, 3, 3, 3, 4, 4]) == [20, 20, 40, 40, 40, 50, 50]

    assert_raise IndexOutOfBoundsError, "index 5 is out of bounds [0-5)",
    fn ->
      get_many_from_list!(list, [0, 1, 5])
    end

    assert_raise IndexOutOfBoundsError, "index 10 is out of bounds [0-5)",
    fn ->
      get_many_from_list!(list, [0, 10, 2])
    end
  end

  test "get_name_from_list sample" do
    list = ["cat", "dog", "fish"]
    assert get_many_from_list(list, [1, 0]) == {:ok, ["dog", "cat"]}
  end

  test "get_many_from_list" do
    list = [:a, :b, :c]
    assert get_many_from_list(list, [0, 1]) == {:ok, [:a, :b]}
    assert get_many_from_list(list, [0, 2, 2]) == {:ok, [:a, :c, :c]}
    assert get_many_from_list(list, [0, 1, 2]) == {:ok, list}
    assert get_many_from_list(list, [2, 2, 0, 0, 1, 1]) == {:ok, [:c, :c, :a, :a, :b, :b]}

    assert get_many_from_list(list, [0, 3, 1]) == {:error, "index 3 is out of bounds [0-3)"}
    assert get_many_from_list(list, [-1, 2]) == {:error, "index -1 is out of bounds [0-3)"}
  end
  
  
end
