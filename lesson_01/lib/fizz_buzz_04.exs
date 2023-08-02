defmodule FizzBuzz04 do
  @moduledoc """
  FizzBuzz is a simple module to show basic usage of Elixir.
  """

  def main() do
    fizzbuzz_100()
    |> Enum.join(" ")
    |> IO.puts()
  end

  @doc "Produces list of strings for numbers from 1 to 100."
  @spec fizzbuzz_100() :: [String.t()]
  def fizzbuzz_100() do
    1..100
    |> Enum.map(&fizzbuzz/1)
  end

  @doc "Produces string result for a single number."
  @spec fizzbuzz(integer) :: String.t()
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
