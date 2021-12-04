defmodule MyList do

  @doc """
  Function takes a list that may contain any number of sublists,
  which themselves may contain sublists, to any depth.
  It returns the elements of these lists as a flat list.

  ## Examples
  iex> MyList.flatten([1, [2, 3], 4, [5, [6, 7, [8, 9, 10]]]])
  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  """
  def flatten([]), do: []

  def flatten([head | tail]), do: flatten(head) ++ flatten(tail)

  def flatten(item), do: [item]


  # ++ operator is not very effective.
  # It would be better to provide a more effective implementation,
  # which is possible, but a little bit tricky.
  def flatten_e(list) do
    # TODO add your implementation
  end

end