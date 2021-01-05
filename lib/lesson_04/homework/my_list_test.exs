ExUnit.start()

defmodule MyListTest do
  use ExUnit.Case
  doctest MyList
  import MyList

  test "flatten" do
    assert flatten([]) == []
    assert flatten([1]) == [1]
    assert flatten([[1]]) == [1]
    assert flatten([[[1]]]) == [1]
    assert flatten([[[[1]]]]) == [1]
    assert flatten([1, [2, 3], 4]) == [1, 2, 3, 4]
    assert flatten([1, [[2, 3]], 4]) == [1, 2, 3, 4]
    assert flatten([[1], [[2, 3]], 4]) == [1, 2, 3, 4]
    assert flatten([[[[[[1]]]]], [[2, 3]], [[[[4]]]]]) == [1, 2, 3, 4]
    assert flatten([[2, 3], 4]) == [2, 3, 4]
    assert flatten([1, [2, 3, [4] ], 5, [[[6]]]]) == [1, 2, 3, 4, 5, 6]
    assert flatten([1, [2, 3], 4, [5, [6, 7, [8, 9, 10]]]]) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    assert flatten([[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, [9, 10]]) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  end


  test "flatten_e" do
    assert flatten_e([]) == []
    assert flatten_e([1]) == [1]
    assert flatten_e([[1]]) == [1]
    assert flatten_e([[[1]]]) == [1]
    assert flatten_e([[[[1]]]]) == [1]
    assert flatten_e([1, [2, 3], 4]) == [1, 2, 3, 4]
    assert flatten_e([1, [[2, 3]], 4]) == [1, 2, 3, 4]
    assert flatten_e([[1], [[2, 3]], 4]) == [1, 2, 3, 4]
    assert flatten_e([[[[[[1]]]]], [[2, 3]], [[[[4]]]]]) == [1, 2, 3, 4]
    assert flatten_e([[2, 3], 4]) == [2, 3, 4]
    assert flatten_e([1, [2, 3, [4] ], 5, [[[6]]]]) == [1, 2, 3, 4, 5, 6]
    assert flatten_e([1, [2, 3], 4, [5, [6, 7, [8, 9, 10]]]]) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    assert flatten_e([[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, [9, 10]]) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  end
  
end
