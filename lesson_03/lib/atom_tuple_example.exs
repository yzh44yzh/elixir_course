defmodule AtomTupleExample do
  def distance({:point, x1, y1}, {:point, x2, y2}) do
    x_dist = abs(x1 - x2)
    y_dist = abs(y1 - y2)
    :math.sqrt(:math.pow(x_dist, 2) + :math.pow(y_dist, 2))
  end

  def point_inside_figure?(point, {:circle, center, radius}) do
    distance(point, center) <= radius
  end

  def point_inside_figure?({:point, x, y}, {:rect, left_top, right_bottom}) do
    {:point, left_x, top_y} = left_top
    {:point, right_x, bottom_y} = right_bottom
    x >= left_x and x <= right_x and y <= top_y and y >= bottom_y
  end
end

ExUnit.start()

defmodule AtomTupleExampleTest do
  use ExUnit.Case
  import AtomTupleExampleTest

  test "distance" do
    assert 5.0 == distance({:point, 0, 0}, {:point, 0, 5})
    assert 5.0 == distance({:point, 5, 0}, {:point, 0, 0})
    assert 0.0 == distance({:point, 5, 5}, {:point, 5, 5})
    assert 5.0 == distance({:point, 0, 0}, {:point, 3, 4})
    assert 5.0 == distance({:point, 0, 0}, {:point, -3, -4})
  end

  test "bigger distance" do
    assert 12.806248474865697 == distance({:point, 2, 2}, {:point, 10, 12})
    assert 21.213203435596427 == distance({:point, -5, -5}, {:point, 10, 10})
    assert 21.400934559032695 == distance({:point, -5, 5}, {:point, 8, -12})
    assert 17.26267650163207 == distance({:point, -5, 5}, {:point, -8, -12})
  end

  test "invalid arguments" do
    assert_raise FunctionClauseError, fn -> distance({0, 0}, {0, 5}) end
  end

  test "point inside circle" do
    point = {:point, 50, 50}
    assert point_inside_figure?(point, {:circle, {:point, 10, 10}, 100})
    assert not point_inside_figure?(point, {:circle, {:point, -10, -10}, 20})
  end

  test "point inside rect" do
    point = {:point, -10, 20}
    assert point_inside_figure?(point, {:rect, {:point, -20, 30}, {:point, 20, 10}})
    assert not point_inside_figure?(point, {:rect, {:point, 0, 0}, {:point, 10, 10}})
  end
end
