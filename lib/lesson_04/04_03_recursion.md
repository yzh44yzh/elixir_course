В Эликсир, как и во всех функциональных языках, нет циклов. Их заменяет рекурсия.

Strategy

Divide and conquer

Decrease and conquer

это из книги LFPwE
TODO посмотреть, какие там есть примеры для каждой стратегии


```
def len([]), do: 0
def len([head|tail]), do: 1 + len(tail)
```

Write a max(list) that returns the element with the maximum value in the list.
(This is slightly trickier than it sounds.)

```
def square([]), do: []
def square([ head | tail ]), do: [ head*head | square(tail) ]
```

TODO Еще неплохо было бы придумать пример опосредованой рекурсии, через 2 функции.
