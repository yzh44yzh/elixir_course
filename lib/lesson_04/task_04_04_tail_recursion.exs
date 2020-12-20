defmodule Lesson_04.Task_04_04_TailRecursion do

  def factorial(0), do: 1
  def factorial(n) when is_integer(n) and n > 0 do
    if rem(n, 1000) == 0 do
      data = :erlang.process_info(self(), [:memory, :heap_size, :total_heap_size, :stack_size])
      IO.puts("memory #{data[:memory]}, heap #{data[:heap_size]}, total heap #{data[:total_heap_size]}, stack #{data[:stack_size]}")
    end
    n * factorial(n - 1)
  end

  def factorial_t(n) do
    factorial_t(n, 1)
  end

  defp factorial_t(0, acc), do: acc
  defp factorial_t(n, acc) do
    if rem(n, 1000) == 0 do
      data = :erlang.process_info(self(), [:memory, :heap_size, :total_heap_size, :stack_size])
      IO.puts("memory #{data[:memory]}, heap #{data[:heap_size]}, total heap #{data[:total_heap_size]}, stack #{data[:stack_size]}")
    end
    factorial_t(n - 1, n * acc)
  end

end