defmodule UnboundedRecursion do

  def browse() do
    browse("/home/yuri/p/elixir_course/lesson_05")
  end

  def browse(path) do
    browse(path, [])
  end

  defp browse(path, acc) do
    cond do
      File.regular?(path) ->
        [path | acc]

      File.dir?(path) ->
        {:ok, items} = File.ls(path)
        new_acc = browse_items(items, path, acc)
        [path | new_acc]
    end
  end

  defp browse_items([], _parent, acc), do: acc

  defp browse_items([item | items], parent, acc) do
    full_path = Path.join(parent, item)
    new_acc = browse(full_path, acc)
    browse_items(items, parent, new_acc)
  end

end
