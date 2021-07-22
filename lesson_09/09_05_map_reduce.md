# Map reduce

```
iex(1)> c "09_05_map_reduce.exs"
[Lesson_09.Task_05_Map_Reduce, Lesson_09.Task_05_Map_Reduce.Worker]
iex(2)> alias Lesson_09.Task_05_Map_Reduce, as: T
Lesson_09.Task_05_Map_Reduce
iex(3)> T.start()
start ["./09_01_processes.md", "./09_02_mailbox.md", "./09_03_link.md", "./09_04_monitor.md"]
start worker #PID<0.120.0> with file './09_01_processes.md'
start worker #PID<0.121.0> with file './09_02_mailbox.md'
start worker #PID<0.122.0> with file './09_03_link.md'
start worker #PID<0.123.0> with file './09_04_monitor.md'
got result 872 from #PID<0.120.0>
got result 644 from #PID<0.121.0>
got result 325 from #PID<0.123.0>
got result 1162 from #PID<0.122.0>
DONE
3003
```
