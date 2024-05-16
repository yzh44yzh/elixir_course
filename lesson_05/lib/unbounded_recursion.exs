defmodule UnboundedRecursion do

  def browse(path) do
    browse(path, "", [])
  end

  defp browse([], _parent, acc), do: acc

  defp browse([path | tail], parent, acc) do
    new_acc = browse(path, parent, acc)
    browse(tail, parent, new_acc)
  end

  defp browse(path, parent, acc) when is_binary(path) do
    full_path = Path.join(parent, path)
    cond do
      File.regular?(full_path) ->
        [full_path | acc]
      File.dir?(full_path) ->
        {:ok, items} = File.ls(full_path)
        new_acc = Enum.sort(items) |> browse(full_path, acc)
        [full_path | new_acc]
      end
  end

end
