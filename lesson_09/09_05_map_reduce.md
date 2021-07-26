# MapReduce

Мы теперь умеем создавать потоки и передавать информацию между ними.

https://ru.wikipedia.org/wiki/MapReduce



```
iex(1)> c "09_05_map_reduce.exs"
iex(2)> alias Lesson_09.Task_05_Map_Reduce, as: T
iex(3)> T.start()
{:ok, 3003}

start reducer 'root_reducer' with childs [:r1, :r2]
start reducer 'r1' with childs [:w1, :w2]
start mapper 'w1' with file './09_01_processes.md'
start mapper 'w2' with file './09_02_mailbox.md'
start reducer 'r2' with childs [:w3, :w4]
start mapper 'w3' with file './09_03_link.md'
start mapper 'w4' with file './09_04_monitor.md'
reducer r1 got result 872 from w1
reducer r1 got result 644 from w2
reducer root_reducer got result 1516 from r1
reducer r2 got result 1162 from w3
reducer r2 got result 325 from w4
reducer root_reducer got result 1487 from r2
```
