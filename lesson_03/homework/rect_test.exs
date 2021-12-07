ExUnit.start()

defmodule RectTest do
  use ExUnit.Case
  import Rect

  test "valid_rect" do
    assert valid_rect({:rect, {:point, 0, 10}, {:point, 10, 0}})
    assert valid_rect({:rect, {:point, -10, 0}, {:point, 0, -10}})
    assert valid_rect({:rect, {:point, -10, 100}, {:point, 100, -10}})
    assert valid_rect({:rect,  {:point, 1, 2}, {:point, 2, 1}})
    assert not valid_rect({:rect, {:point, 1, 1}, {:point, 2, 2}})
    assert not valid_rect({:rect, {:point, 2, 1}, {:point, 1, 2}})
  end

  test "intersect case 1" do
    #   -----
    #  |     |
    #   -----
    #     -----
    #    |     |
    #     -----
    rect_1 = {:rect, {:point, 3, 3}, {:point, 4, 1}}
    rect_2 = {:rect, {:point, 1, 4}, {:point, 2, 2}}
    assert not intersect(rect_1, rect_2)
    assert not intersect(rect_2, rect_1)
  end

  test "intersect case 2" do
    #   -------
    #  |    ---|---
    #   ---|---    |
    #       -------
    rect_1 = {:rect, {:point, 2, 3}, {:point, 4, 1}}
    rect_2 = {:rect, {:point, 1, 4}, {:point, 3, 2}}
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
    rect_1 = {:rect, {:point, 2, 4}, {:point, 3, 1}}
    rect_2 = {:rect, {:point, 1, 3}, {:point, 4, 2}}
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
    rect_1 = {:rect, {:point, 1, 4}, {:point, 4, 1}}
    rect_2 = {:rect, {:point, 2, 3}, {:point, 3, 2}}
    assert intersect(rect_1, rect_2)
    assert intersect(rect_2, rect_1)
  end

  test "intersect invalid rect" do
    rect_v = {:rect, {:point, 3, 3}, {:point, 4, 1}}
    rect_i = {:rect, {:point, 1, 1}, {:point, 2, 2}}
    assert_raise RuntimeError, "invalid rect 2", fn -> intersect(rect_v, rect_i) end
    assert_raise RuntimeError, "invalid rect 1", fn -> intersect(rect_i, rect_v) end
  end
end
