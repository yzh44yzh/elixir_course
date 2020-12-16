defmodule Lesson_04.Task_04_03_Recursion do

  def len([]), do: 0
  def len([_head | tail]) do
    1 + len(tail)
  end

  def list_max([]), do: nil
  def list_max([elem]), do: elem
  def list_max([head | tail]) do
    max(head, list_max(tail))
  end

end
