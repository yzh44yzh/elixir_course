defmodule TailRecursion do

  def factorial(0), do: 1
  def factorial(n) when is_integer(n) and n > 0 do
    if rem(n, 1000) == 0, do: report_memory()
    n * factorial(n - 1)
  end

  def factorial_t(n) do
    factorial_t(n, 1)
  end

  defp factorial_t(0, acc), do: acc
  defp factorial_t(n, acc) do
    if rem(n, 1000) == 0, do: report_memory()
    factorial_t(n - 1, n * acc)
  end


  def fibonacci(0), do: 0
  def fibonacci(1), do: 1
  def fibonacci(n) do
    report_memory()
    fibonacci(n - 1) + fibonacci(n - 2)
  end

  def fibonacci_t(0), do: 0
  def fibonacci_t(1), do: 1
  def fibonacci_t(n) do
    report_memory()
    fibonacci_t(n, 2, {0, 1})
  end

  def fibonacci_t(till, step, {fn_2, fn_1}) when till == step do
    report_memory()
    fn_2 + fn_1
  end
  def fibonacci_t(till, step, {fn_2, fn_1}) do
    fibonacci_t(till, step + 1, {fn_1, fn_1 + fn_2})
  end


  def sum([]), do: 0
  def sum([head | tail]) do
    report_memory()
    head + sum(tail)
  end

  def sum_t(list) do
    sum_t(list, 0)
  end

  def sum_t([], acc), do: acc
  def sum_t([head | tail], acc) do
    report_memory()
    sum_t(tail, head + acc)
  end

  # http://erlang.org/documentation/doc-5.7.4/erts-5.7.4/doc/html/erlang.html#process_info-1
  # memory is measured in bytes, but head and stack are measured in machine words
  # which is 4 bytes for 64 bit arch
  defp report_memory() do
    data = :erlang.process_info(self(), [:memory, :total_heap_size, :stack_size])
    IO.puts("memory #{data[:memory]}, heap #{data[:total_heap_size]}, stack #{data[:stack_size]}")
  end

end
