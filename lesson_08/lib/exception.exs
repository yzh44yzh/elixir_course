defmodule Lesson_08.Exception do

  def try_rescue() do
    try do
      # :a = :b
      # 42 + :a
      # raise(RuntimeError)
      apply(SameModule, :some_fun, [])
    rescue
      error in [MatchError, ArithmeticError] ->
        IO.puts("This is MatchError or ArithmeticError: #{inspect error}")
      error in [RuntimeError] ->
        IO.puts("This is RuntimeError: #{inspect error}")
      error ->
        IO.puts("unknown error: #{inspect error}")
    after
      IO.puts("after clause is always called")
    end
  end
  
end
