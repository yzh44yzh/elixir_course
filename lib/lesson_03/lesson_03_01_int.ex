defmodule Lesson_03_01_Int do

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

# TODO unit test

# tests:
#iex(5)> Lesson_03_01_Int.gcd(12, 9)
#3
#iex(6)> Lesson_03_01_Int.gcd(9, 12)
#3
#iex(25)> Lesson_03_01_Int.gcd(60, 48)
#12
#iex(7)> Lesson_03_01_Int.gcd(24, 18)
#6
#iex(10)> Lesson_03_01_Int.gcd(24, -18)
#6
#iex(11)> Lesson_03_01_Int.gcd(-24, -18)
#6
#iex(12)> Lesson_03_01_Int.gcd(-24, 18)
#6
#iex(18)> Lesson_03_01_Int.gcd(24, 0)
#24
#iex(18)> Lesson_03_01_Int.gcd(0, 18)
#18
#iex(18)> Lesson_03_01_Int.gcd(0, 0)
#0




