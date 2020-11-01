defmodule Lesson_03.Task_03_04_AtomTuple do

  def distance({:point, x1, y1}, {:point, x2, y2}) do
    x_dist = abs(x1 - x2)
    y_dist = abs(y1 - y2)
    :math.sqrt(:math.pow(x_dist, 2) + :math.pow(y_dist, 2))
  end

end

ExUnit.start()

defmodule Task_04_Test do
  use ExUnit.Case
  import Lesson_03.Task_03_04_AtomTuple

  test "distance" do
    assert 5.0 == distance({:point, 0, 0}, {:point, 0, 5})
    assert 5.0 == distance({:point, 5, 0}, {:point, 0, 0})
    assert 0.0 == distance({:point, 5, 5}, {:point, 5, 5})
    assert 5.0 == distance({:point, 0, 0}, {:point, 3, 4})
    assert 5.0 == distance({:point, 0, 0}, {:point, -3, -4})
  end

  test "bigger distance" do
    assert 12.806248474865697 == distance({:point, 2, 2},   {:point, 10, 12})
    assert 21.213203435596427 == distance({:point, -5, -5}, {:point, 10, 10})
    assert 21.400934559032695 == distance({:point, -5, 5},  {:point, 8, -12})
    assert 17.26267650163207  == distance({:point, -5, 5},  {:point, -8, -12})
  end

  test "invalid arguments" do
    assert_raise FunctionClauseError, fn -> distance({0, 0}, {0, 5}) end
  end

end