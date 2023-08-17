defmodule ListExample do
  @doc """
  Операция merge из алгоритма Merge Sort.
  """
  @spec merge(list(), list()) :: list()
  def merge(list1, list2) do
    merge(list1, list2, [])
  end

  defp merge([], list2, acc), do: Enum.reverse(acc) ++ list2
  defp merge(list1, [], acc), do: Enum.reverse(acc) ++ list1

  defp merge(list1, list2, acc) do
    [head1 | tail1] = list1
    [head2 | tail2] = list2

    if head1 < head2 do
      merge(tail1, list2, [head1 | acc])
    else
      merge(list1, tail2, [head2 | acc])
    end
  end
end

ExUnit.start()

defmodule ListExampleTest do
  use ExUnit.Case
  import ListExample

  test "merge" do
    assert [1, 2, 3, 4, 5] == merge([1, 3], [2, 4, 5])
    assert [-100, 0, 2, 22, 55, 500, 1000] == merge([2, 22, 500, 1000], [-100, 0, 55])
  end

  test "merge, corner cases" do
    assert [] == merge([], [])
    assert [1, 2, 3] == merge([], [1, 2, 3])
    assert [1, 2, 3] == merge([1, 2, 3], [])
  end
end
