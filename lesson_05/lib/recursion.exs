defmodule Recursion do

  def len([]), do: 0
  def len([_head | tail]) do
    1 + len(tail)
  end

  def list_max([]), do: nil
  def list_max([elem]), do: elem
  def list_max([head | tail]) do
    max(head, list_max(tail))
  end

  def range(from, to) when from <= to do
    [from | range(from + 1, to)]
  end
  def range(_, _), do: []

  def swap_pair([]), do: []
  def swap_pair([_]), do: raise "Can't swap a list with an odd number of elements"
  def swap_pair([a, b | tail]) do
    [b, a | swap_pair(tail)]
  end

end
