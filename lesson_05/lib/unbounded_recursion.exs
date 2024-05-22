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

  def browse_with_limit(limit) do
    browse_with_limit("/home/yuri/p/elixir_course/lesson_05", limit)
  end

  def browse_with_limit(path, limit) do
    acc = %{
      current_depth: 0,
      limit: limit,
      items: []
    }

    acc = do_browse_with_limit(path, acc)
    acc.items
  end

  defp do_browse_with_limit(_path, %{
         current_depth: limit,
         limit: limit} = acc
       ), do: acc

  defp do_browse_with_limit(path, acc) do
    items =
      cond do
        File.regular?(path) ->
          [path | acc.items]

        File.dir?(path) ->
          {:ok, items} = File.ls(path)
          acc = %{acc | current_depth: acc.current_depth + 1}
          acc = do_browse_items(items, path, acc)
          [path | acc.items]
      end

    %{acc | items: items}
  end

  defp do_browse_items([], _parent, acc), do: acc

  defp do_browse_items([item | items], parent, acc) do
    full_path = Path.join(parent, item)
    acc = do_browse_with_limit(full_path, acc)
    do_browse_items(items, parent, acc)
  end

end
