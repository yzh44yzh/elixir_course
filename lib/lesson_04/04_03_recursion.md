# Рекурсия

В Эликсир, как и во всех функциональных языках, нет циклов. Их заменяет рекурсия.

```
def len([]), do: 0
def len([head|tail]), do: 1 + len(tail)
```

Функция из двух тел: 
- условие выхода из рекурсии;
- один шаг итерации.


Write a max(list) that returns the element with the maximum value in the list.
(This is slightly trickier than it sounds.)

```
def square([]), do: []
def square([ head | tail ]), do: [ head*head | square(tail) ]
```

факториал
```
defmodule Factorial do

  def of(0), do: 1
  def of(n) when is_integer(n) and n > 0, do: n * of(n - 1)

end
```

## Стратегии рекурсии

TODO: поискать больше инфы по этой теме. В UA не особо хорошо описано.

**Decrease and conquer**

Постепенное уменьшение задачи шаг за шагом, приходящее к тривиальному случаю.

Все примеры выше относятся к этой стратегии.


**Divide and conquer**

Разделяем задачу на две (или больше) частей, каждую из которых можно решать независимо, затем объединяем полученные результаты. 

Пример: Merge sort.


## Еще Примеры

create range
Write a function MyList.span(from, to) that returns a list of the numbers from from up to to .

Swaps pairs of values in a list:
```
defmodule Swapper do
  def swap([]), do: []
  def swap([ a, b | tail ]), do: [ b, a | swap(tail) ]
  def swap([_]), do: raise "Can't swap a list with an odd number of elements"
end
```

TODO Еще неплохо было бы придумать пример опосредованой рекурсии, через 2 функции.
