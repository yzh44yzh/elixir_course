### String, Binary
два вида строк
Inspect

wc -- посчитать количество строк, слов и букв в файле.

Write a function that takes a list of double-quoted strings and prints each
on a separate line, centered in a column that has the width of the longest
string. Make sure it works with UTF characters.
iex> center(["cat", "zebra", "elephant"])
  cat
 zebra
elephant

draw ASCII table
|  a |  b |  c |
| 11 | 12 | 13 |
можно сделать что-то простое, и предложить более сложное как упражнение


```
iex(1)> c "lib/lesson_03/task_03_07_string.exs"
iex(2)> words = ~w'cat zebra elephant'
iex(3)> Enum.map(words, &String.length/1)
```