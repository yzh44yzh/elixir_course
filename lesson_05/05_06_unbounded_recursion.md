## Неограниченная рекурсия (Unbounded recursion)

Ограниченная рекурсия (bounded recursion) -- это случай, когда число шагов (итераций) известно.

В случае неограниченной рекурсии (unbounded recursion) мы не можем предсказать число итераций. Например, мы реализуем веб-паука, который скачивает веб-страницу, а затем проходит по всем ссылкам на этой странице. Другой пример -- обход файловой системы по дереву каталогов.

Рекурсивно обходим указанный каталог, получаем список файлов вместе с их глубиной:

```elixir
$ iex lib/unbounded_recursion.exs
> alias UnboundedRecursion, as: R
> files = R.browse("/path/to/lesson_04")
[
  {"lesson_04", 0},
  {"04_01_pattern_matching.md", 1},
  {"04_02_pattern_matching_for_maps.md", 1},
  {"04_03_case.md", 1},
  {"04_04_cond.md", 1},
  {"04_05_do_end.md", 1},
  {"homework", 1},
  {"Elixir.Game.beam", 2},
  {"Elixir.TicTacToe.beam", 2},
  {"README.md", 2},
  {"game.exs", 2},
  {"game_test.exs", 2},
  {"test.sh", 2},
  {"tic_tac_toe.exs", 2},
  {"tic_tac_toe_test.exs", 2},
  {"lib", 1},
  {"control_flow.exs", 2},
  {"do_end.exs", 2}
]
```

Рендерим полученный список в виде дерева:

```elixir
> res = R.render(files)
" lesson_04\n|-- 04_01_pattern_matching.md\n|-- 04_02_pattern_matching_for
_maps.md\n|-- 04_03_case.md\n|-- 04_04_cond.md\n|-- 04_05_do_end.md\n|-- h
omework\n|--|-- Elixir.Game.beam\n|--|-- Elixir.TicTacToe.beam\n|--|-- REA
DME.md\n|--|-- game.exs\n|--|-- game_test.exs\n|--|-- test.sh\n|--|-- tic_
tac_toe.exs\n|--|-- tic_tac_toe_test.exs\n|-- lib\n|--|-- control_flow.exs
\n|--|-- do_end.exs\n"

> IO.puts(res)
 lesson_04
|-- 04_01_pattern_matching.md
|-- 04_02_pattern_matching_for_maps.md
|-- 04_03_case.md
|-- 04_04_cond.md
|-- 04_05_do_end.md
|-- homework
|--|-- Elixir.Game.beam
|--|-- Elixir.TicTacToe.beam
|--|-- README.md
|--|-- game.exs
|--|-- game_test.exs
|--|-- test.sh
|--|-- tic_tac_toe.exs
|--|-- tic_tac_toe_test.exs
|-- lib
|--|-- control_flow.exs
|--|-- do_end.exs

```

В случае unbounded recursion нам могут понадобится гарантии, что рекурсия не будет бесконечной и когда-либо завешится. При движении по направленному графу (это и обход ссылок в вебе, и обход каталогов в файловой системе) там могут быть циклические связи. Например, страница А ссылается на страницу Б, а страница Б ссылается на страницу А. Причем это может быть не напрямую, а через промежуточные связи. В файловой системе аналогичную ситуацию можно сделать с помощью символических ссылок.

Тут придется принять дополнительные меры, чтобы не попасть в бесконечную рекурсию. Меры могут быть разными. Можно просто ограничить число шагов рекурсии, и остановиться. Можно игнорировать узлы определенного типа (например, символические ссылки в файловой системе). Можно сохранять пройденные узлы и проверять каждый новый узел, не были ли мы здесь раньше.

Для обхода файловой системы можно применять все три способа.
