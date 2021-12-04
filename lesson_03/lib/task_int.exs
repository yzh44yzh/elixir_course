defmodule Lesson_03.Task_03_01_Int do

  # ignore sign
  def gcd(a, b) when a < 0, do: gcd(-a, b)
  def gcd(a, b) when b < 0, do: gcd(a, -b)

  # deal with 0
  def gcd(0, b), do: b
  def gcd(a, 0), do: a

  def gcd(a, b) do
    case rem(a, b) do
      0 -> b
      c -> gcd(b, c)
    end
  end

end

ExUnit.start()

defmodule Task_03_01_Test do
  use ExUnit.Case
  import Lesson_03.Task_03_01_Int

  test "gcd" do
    assert gcd(12, 9) == 3
    assert gcd(9, 12) == 3
    assert gcd(60, 48) == 12
  end

  test "gcd with negative numbers" do
    assert gcd(24, 18) == 6
    assert gcd(24, -18) == 6
    assert gcd(-24, -18) == 6
    assert gcd(-24, 18) == 6
  end

  test "gcd with zero" do
    assert gcd(24, 0) == 24
    assert gcd(0, 18) == 18
    assert gcd(0, 0) == 0
  end
end
