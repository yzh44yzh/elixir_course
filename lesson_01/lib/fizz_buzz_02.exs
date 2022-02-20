defmodule FizzBuzz02 do
  @moduledoc """
  https://ru.wikipedia.org/wiki/Fizz_buzz
  """


  def main() do
    1..100
    |> Enum.map(&fizzbuzz/1)
    |> Enum.join(" ")
    |> IO.puts
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
