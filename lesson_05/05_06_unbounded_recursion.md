## Неограниченная рекурсия (Unbounded recursion)

Те примеры рекурсии, которые мы видели раньше, в реальных проектах почти не встречаются, так как хорошей практикой является использование функций высшего порядка из модуля Enum. Но бывают случаи, когда нужно выйти за рамки библиотечных функций и написать рекурсию явно. Поэтому важно уметь это делать.

Ограниченная рекурсия (bounded recursion) -- это случай, когда число шагов (итераций) известно. Что справедливо для всех стандартных коллекций -- списков, словарей и строк.

В случае неограниченной рекурсии (unbounded recursion) мы не можем предсказать число итераций. Например, мы реализуем веб-паука, который скачивает веб-страницу, а затем проходит по всем ссылкам на этой странице. Другой пример -- обход файловой системы по дереву каталогов.

Давайте реализуем вторую задачу.

Мы получаем на вход каталог и на выходе отдаём список всех файлов, включая вложенные каталоги. (Похожую задачу выполняет библиотечная функция `Path.wildcard/2`).

```
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
```

Эта реализация интересна тем, что у нас рекурсия перемещается между двумя разными функциями, но обе они используют один и тот же аккумулятор.

Можно было бы использовать одну функцию, которая по-разному работает с одним элементом и со списком элеметнов. Но мне кажется, что в данном случае будет нагляднее использовать две разные функции.


```elixir-iex
$ iex lib/unbounded_recursion.exs
iex(1)> alias UnboundedRecursion, as: R
iex(2)> R.browse("/home/yuri/p/elixir_course/lesson_05")
```

В случае unbounded recursion нам нужны гарантии, что рекурсия не будет бесконечной и когда-либо завершится. При движении по направленному графу (это и обход ссылок в вебе, и обход каталогов в файловой системе) там могут быть циклические связи.

Например, страница А ссылается на страницу Б, а страница Б ссылается на страницу А. Причем это может быть не напрямую, а через промежуточные связи. В файловой системе аналогичную ситуацию можно сделать с помощью символических ссылок.

Тут придется принять дополнительные меры, чтобы не попасть в бесконечную рекурсию. Меры могут быть разными. Можно просто ограничить число шагов рекурсии и остановиться. Можно игнорировать узлы определенного типа (например, символические ссылки в файловой системе). Можно сохранять пройденные узлы и проверять каждый новый узел, не был ли он обработан раньше.

Давайте сделаем обход файловой системы с ограничением глубины. Идея простая -- передавать в параметрах лимит глубины и текущую глубину.

Мы можем либо добавить два новых аргумента, которые нужно пробрасывать через все функции, либо сделать сложный аккумулятор, который будет хранить в себе всё необходимое.

Идея простая, но код становится сложнее:

```
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
```

Вот это тело функции:
```
  defp do_browse_with_limit(_path, %{
         current_depth: limit,
         limit: limit} = acc
       ), do: acc
```
это выход из рекурсии при достижении нужной глубины.

А вот здесь:
```
  acc = %{acc | current_depth: acc.current_depth + 1}
  acc = do_browse_items(items, path, acc)
```
мы увеличиваем текущую глубину.

В остальном реализация остаётся такой же.
