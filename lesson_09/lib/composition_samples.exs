defmodule Lesson_08.Task_01_CompositionSamples do

  @spec f1(integer()) :: integer()
  def f1(a) do
    a + 1
  end

  @spec f2(integer()) :: integer()
  def f2(a) do
    a + 10
  end

  @spec f3(integer()) :: {:ok, integer()}
  def f3(a) do
    {:ok, a + 1}
  end

  @spec f4(integer()) :: {:ok, integer()} | {:error, atom()}
  def f4(a) do
    if a > 10 do
      {:ok, a + 1}
    else
      {:error, :below_limit}
    end
  end

  def f5(a) do
    drop_database()
    a + 1
  end
  
  def main() do
    # 42 |> f1 |> f2
    # 42 |> f3 |> elem(1) |> f2
    # 42 |> f4 |> elem(1) |> f2
    # 2 |> f4 |> elem(1) |> f2
    42 |> f5 |> f2
  end

end
