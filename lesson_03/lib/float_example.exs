defmodule FloatExample do
  def equal?(f1, f2, precision \\ 0.01) do
    abs(f1 - f2) < precision
  end
end

ExUnit.start()

defmodule FloatExampleTest do
  use ExUnit.Case
  import FloatExample

  test "equal?" do
    assert equal?(3.5, 3.5)
    assert equal?(3.51, 3.51)
    assert not equal?(3.51, 3.53)
  end

  test "equal? with precision" do
    assert equal?(3.5, 3.5, 0.01)
    assert equal?(3.51, 3.51, 0.01)
    assert not equal?(3.51, 3.53, 0.01)
    assert equal?(3.51, 3.53, 0.1)
    assert not equal?(3.501, 3.503, 0.001)
    assert equal?(3.501, 3.503, 0.01)
  end

  test "equal? with negative numbers" do
    assert equal?(-7.77, -7.75, 0.1)
    assert equal?(-10.95, -11.0, 0.2)
    assert equal?(-10.95, -11.0, 0.06)
    assert not equal?(-10.95, -11.0, 0.02)
  end
end
