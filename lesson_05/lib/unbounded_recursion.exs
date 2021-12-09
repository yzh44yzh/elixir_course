defmodule UnboundedRecursion do

  def browse(path) do
    browse(path, 0)
  end

  defp browse(path, depth) do
    children = if File.dir?(path) do
      {:ok, items} = File.ls(path)
      items
      |> Enum.sort
      |> browse_list(path, depth + 1, [])
    else
      []
    end
    current = {Path.basename(path), depth}
    [current | children]
  end

  defp browse_list([], _, _, acc), do: acc
  defp browse_list([item | items], path, depth, acc) do
    children = browse(Path.join(path, item), depth)
    browse_list(items, path, depth, acc ++ children)
  end

  def render([]), do: ""
  def render([file | tail]) do
    render_file(file) <> "\n" <> render(tail)
  end

  def render_file({name, depth}) do
    render_depth(depth) <> " " <> name
  end

  def render_depth(0), do: ""
  def render_depth(n) do
    "|--" <> render_depth(n - 1)
  end

end
