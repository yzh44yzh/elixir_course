defmodule Lesson_04.Task_04_06_UnboundedRecursion do

  def browse(path) do
    IO.puts(path)
    if File.dir?(path) do
      {:ok, items} = File.ls(path)
      Enum.each(items, fn item -> browse(Path.join(path, item)) end)
    end
  end

  # TODO: show as tree with left padding
  def tree(path) do
  end

  # TODO: limit depth
  def tree(path, depth_limit) do
  end

end
