defmodule FizzBuzz03 do

  def main() do
    fizzbuzz_100()
    |> Enum.join(" ")
    |> IO.puts()
  end


  @spec fizzbuzz_100() :: [String.t]
  def fizzbuzz_100() do
    1..100
    |> Enum.map(&fizzbuzz/1)
  end


  @spec fizzbuzz(integer) :: String.t
  def fizzbuzz(n) do
    divisible_by_3 = rem(n, 3) == 0
    divisible_by_5 = rem(n, 5) == 0
    cond do
      divisible_by_3 and divisible_by_5 -> "FizzBuzz"
      divisible_by_3 -> "Fizz"
      divisible_by_5 -> "Buzz"
      true -> to_string(n)
    end
  end

end

ExUnit.start()

defmodule FizzBuzzTest do
  use ExUnit.Case
  import FizzBuzz03

  test "fizzbuzz 1-10" do
    assert fizzbuzz(1) == "1"
    assert fizzbuzz(2) == "2"
    assert fizzbuzz(3) == "Fizz"
    assert fizzbuzz(4) == "4"
    assert fizzbuzz(5) == "Buzz"
    assert fizzbuzz(6) == "Fizz"
    assert fizzbuzz(7) == "7"
    assert fizzbuzz(8) == "8"
    assert fizzbuzz(9) == "Fizz"
    assert fizzbuzz(10) == "Buzz"
  end

  test "fizzbuzz 15, 30" do
    assert fizzbuzz(15) == "FizzBuzz"
    assert fizzbuzz(30) == "FizzBuzz"
  end

  test "fizzbuzz_100" do
    res = fizzbuzz_100()
    assert Enum.take(res, 5) == ["1", "2", "Fizz", "4", "Buzz"]
    assert res |> Enum.drop(9) |> Enum.take(6) == ["Buzz", "11", "Fizz", "13", "14", "FizzBuzz"]
  end
end
