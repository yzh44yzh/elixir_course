ExUnit.start()

defmodule RectTest do
  use ExUnit.Case
  import Rect

  test "valid_rect" do
    assert valid_rect({:rect, {:point, 0, 0}, {:point, 10, 10}})
    assert valid_rect({:rect, {:point, -10, -10}, {:point, 0, 0}})
    assert valid_rect({:rect, {:point, -10, -10}, {:point, 100, 100}})
    assert valid_rect({:rect, {:point, 1, 1}, {:point, 2, 2}})
    assert not valid_rect({:rect, {:point, 2, 2}, {:point, 1, 1}})
    assert not valid_rect({:rect, {:point, 1, 2}, {:point, 2, 1}})
  end

  test "intersect case 1" do
    #   -----
    #  |     |
    #   -----
    #     -----
    #    |     |
    #     -----
    rect_1 = {:rect, {:point, 3, 1}, {:point, 4, 3}}
    rect_2 = {:rect, {:point, 1, 2}, {:point, 2, 4}}
    assert not intersect(rect_1, rect_2)
    assert not intersect(rect_2, rect_1)
  end

  test "intersect case 2" do
    #   -------
    #  |    ---|---
    #   ---|---    |
    #       -------
    rect_1 = {:rect, {:point, 2, 1}, {:point, 4, 3}}
    rect_2 = {:rect, {:point, 1, 2}, {:point, 3, 4}}
    assert intersect(rect_1, rect_2)
    assert intersect(rect_2, rect_1)
  end

  test "intersect case 3" do
    #      ----
    #     |    |
    #   ----------
    #  |  |    |  |
    #   ----------
    #     |    |
    #      ----
    rect_1 = {:rect, {:point, 2, 1}, {:point, 3, 4}}
    rect_2 = {:rect, {:point, 1, 2}, {:point, 4, 3}}
    assert intersect(rect_1, rect_2)
    assert intersect(rect_2, rect_1)
  end

  test "intersect case 4" do
    #    -----------
    #   |           |
    #   |   -----   |
    #   |  |     |  |
    #   |   -----   |
    #   |           |
    #    -----------
    rect_1 = {:rect, {:point, 1, 1}, {:point, 4, 4}}
    rect_2 = {:rect, {:point, 2, 2}, {:point, 3, 3}}
    assert intersect(rect_1, rect_2)
    assert intersect(rect_2, rect_1)
  end

  test "intersect invalid rect" do
    rect_v = {:rect, {:point, 3, 1}, {:point, 4, 3}}
    rect_i = {:rect, {:point, 2, 2}, {:point, 1, 1}}
    assert_raise RuntimeError, "invalid rect 2", fn -> intersect(rect_v, rect_i) end
    assert_raise RuntimeError, "invalid rect 1", fn -> intersect(rect_i, rect_v) end
  end
end
