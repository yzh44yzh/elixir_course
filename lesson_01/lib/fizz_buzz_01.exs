defmodule FizzBuzz01 do
  @moduledoc """
  https://ru.wikipedia.org/wiki/Fizz_buzz
  """


  def main() do
    Enum.each(1..100, &fizzbuzz/1)
  end


  def fizzbuzz(n) do
    cond do
      rem(n, 3) == 0 and rem(n, 5) == 0 -> IO.puts("FizzBuzz")
      rem(n, 3) == 0 -> IO.puts("Fizz")
      rem(n, 5) == 0 -> IO.puts("Buzz")
      true -> IO.puts(n)
    end
  end

end
