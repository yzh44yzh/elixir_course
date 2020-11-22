# String, Binary

два вида строк

## Char list

Single-quoted strings are represented as a list of integer values, 
each value corresponding to a codepoint in the string. 

## Binary

Double-Quoted Strings Are Binaries

The binary type represents a sequence of bits.
A binary literal looks like << term,... >> .

The simplest term is just a number from 0 to 255. 
The numbers are stored as successive bytes in the binary.


## задача align words:
```
iex(1)> c "lib/lesson_03/task_03_07_string.exs"
iex(2)> words = ~w'cat zebra elephant'
iex(3)> Enum.map(words, &String.length/1)
```

Тесты
```
elixir lib/lesson_03/task_03_07_string.exs
```

Упражнения:

wc -- посчитать количество строк, слов и букв в файле.

draw ASCII table
|  a |  b |  c |
| 11 | 12 | 13 |
можно сделать что-то простое, и предложить более сложное как упражнение