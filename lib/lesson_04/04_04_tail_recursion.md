# Хвостовая рекурсия

Надо понимать, что рекурсия -- штука не бесплатная. Каждый рекурсивный вызов требует сохранения на стеке предыдущего состояния функции, чтобы вернуться в него после очередного вызова.

И хотя стек в BEAM легковесный и позволяет делать миллионы рекурсивных вызовов, (а не тысячи, как в большинстве мейнстримовых языков), он, все-таки, конечный.

Поэтому в Эликсир, как и во многих других функциональных языках, компилятор делает одну полезную штуку, которая называется **оптимизация хвостовой рекурсии**.

Если рекурсивный вызов является последней строчкой кода в данной функции, и после него больше никаких инструкций нет, то такой вызов называется хвостовым. В этом случае нет необходимости возвращаться по стеку во все вызывающие
функции, а можно сразу отдать результат из последнего вызова. И стек не растет, а используется заново каждым новым вызовом, и адрес возврата в нем не меняется.

Это позволяет делать бесконечную рекурсию, которая нужна для бесконечно живущих процессов.  А такие процессы нужны серверам :)

Значит ли это, что всегда нужно всегда использовать хвостовую рекурсию? Нет. 

Код без хвостовой рекурсии часто получается короче и проще, а иногда и лучше по производительности. Если нам нужна бесконечная, или очень долгая рекурсия (миллионы итераций), то без хвостовой рекурсии не обойтись. Но если мы уверены, что у нас будет конечное и не очень большое число итераций, то можно выбрать вариант без неё.


## Пример

Давайте рассмотрим две реализации факториала.

```
iex(1)> 
c "lib/lesson_04/task_04_04_tail_recursion.exs"
[Lesson_04.Task_04_04_TailRecursion] 
iex(2)> 
alias Lesson_04.Task_04_04_TailRecursion, as: TTR
Lesson_04.Task_04_04_TailRecursion
iex(3)> TTR.factorial(10)
3628800
iex(4)> TTR.factorial_t(10)
3628800
iex(5)>
```

Первая реализация без хвостовой рекурсии, и код у нее очень простой. Вторая реализация использует специальный подход "рекурсия с аккумулятором", что позволяет получить хвостовую рекурсию за счет того, что промежуточные результаты сохраняются не на стеке, а в специальном аргументе функции, который называется "аккумулятор".

Мы подробнее рассмотрим рекурсию с аккумулятором в следущей теме. А сейчас сравним обе реализации по тому, как они расходуют память.


## Расход памяти

В книге Learn Functional Programming with Elixir by Ulisses Almeida описываются две реализации факториала, с хвостовой рекурсией и без нее. И они сравниваются по потреблению памяти, по данным System Monitor для процесса beam.smp. 

У автора получилось, что в реализации без хвостовой рекурсии процесс beam.smp при вычислении факториала 10_000_000 потреблял 756Mb памяти. А с хвостовой рекурсией -- 56Mb. Разница впечатляющая. 

Я попробовал воспроизвести эти результаты. У меня OTP 23.2 (у автора версия не указана). 

Получилось так:

Без хвостовой рекурсии:
factorial(20_000) - 96Mb
factorial(40_000) - 113Mb
factorial(100_000) - 1.5G,
factorial(200_000) - 2.5G, не вычислил, beam убит, вероятно ООМ киллером. (У меня на ноуте 16Gb памяти, но свободной было 3Gb). 

С хвостовой рекурсией:
factorial(20_000) - 96Mb
factorial(40_000) - 200Mb
factorial(100_000) - 1.3G

На уровне beam разница небольшая, результаты трудно интерпретировать.

Давайте проследим, как потребляет память конкретный процесс во время вычисления факториала, на каждом шаге рекурсии.

iex(10)> TTR.factorial(10000)
process memory 145072
process memory 145072
process memory 230736
process memory 230736
process memory 230736
process memory 230736
process memory 230736
process memory 230736
process memory 230736
process memory 284912

iex(13)> TTR.factorial_t(10000)
process memory 284972
process memory 1125172
process memory 2746268
process memory 4366716
process memory 5963116
process memory 7531508
process memory 9066884
process memory 10561676
process memory 12024468
process memory 13391252

iex(3)> TTR.factorial(10_000)
memory 26556, heap 1598, stack 57
memory 67948, heap 6772, stack 5057
memory 101436, heap 10958, stack 10057
memory 155620, heap 17731, stack 15057
memory 243292, heap 28690, stack 20057
memory 243292, heap 28690, stack 25057
memory 385148, heap 46422, stack 30057
memory 385148, heap 46422, stack 35057
memory 385148, heap 46422, stack 40057
memory 385148, heap 46422, stack 45057

iex(5)> TTR.factorial_t(10_000)
memory 55164, heap 6772, stack 58
memory 895364, heap 6772, stack 58
memory 4285700, heap 28690, stack 58
memory 5906148, heap 28690, stack 58
memory 7502548, heap 28690, stack 58
memory 9070940, heap 28690, stack 58
memory 10606316, heap 28690, stack 58
memory 12101108, heap 28690, stack 58
memory 13543204, heap 28690, stack 58
memory 14909988, heap 28690, stack 58

iex(9)> TTR.factorial(20_000)  
memory 1804080, heap 28690, total heap 225358, stack 58
memory 1804080, heap 28690, total heap 225358, stack 6058
memory 1804080, heap 28690, total heap 225358, stack 12058
memory 1804080, heap 28690, total heap 225358, stack 18058
memory 1945792, heap 46422, total heap 243072, stack 24058
memory 1945792, heap 46422, total heap 243072, stack 30058
memory 1945792, heap 46422, total heap 243072, stack 36058
memory 1945792, heap 46422, total heap 243072, stack 42058
memory 2175320, heap 75113, total heap 271763, stack 48058
memory 2175320, heap 75113, total heap 271763, stack 54058
memory 2175320, heap 75113, total heap 271763, stack 60058
memory 2175320, heap 75113, total heap 271763, stack 66058
memory 2175320, heap 75113, total heap 271763, stack 72058
memory 2546704, heap 121536, total heap 318186, stack 78058
memory 2546704, heap 121536, total heap 318186, stack 84058
memory 2546704, heap 121536, total heap 318186, stack 90058
memory 2546704, heap 121536, total heap 318186, stack 96058
memory 2546704, heap 121536, total heap 318186, stack 102058
memory 2546704, heap 121536, total heap 318186, stack 108058
memory 2546704, heap 121536, total heap 318186, stack 114058

iex(11)> TTR.factorial_t(20_000)
memory 1803936, heap 28690, total heap 225340, stack 59
memory 2708288, heap 28690, total heap 338384, stack 59
memory 4484928, heap 28690, total heap 560464, stack 59
memory 6251856, heap 28690, total heap 781330, stack 59
memory 8008480, heap 28690, total heap 1000908, stack 59
memory 9754160, heap 28690, total heap 1219118, stack 59
memory 11488144, heap 28690, total heap 1435866, stack 59
memory 13209728, heap 28690, total heap 1651064, stack 59
memory 14917928, heap 28690, total heap 1864589, stack 59
memory 16611656, heap 28690, total heap 2076305, stack 59
memory 18289680, heap 28690, total heap 2286058, stack 59
memory 19950496, heap 28690, total heap 2493660, stack 59
memory 21592288, heap 28690, total heap 2698884, stack 59
memory 23212880, heap 28690, total heap 2901458, stack 59
memory 24809072, heap 28690, total heap 3100982, stack 59
memory 26377512, heap 28690, total heap 3297037, stack 59
memory 27912888, heap 28690, total heap 3488959, stack 59
memory 29407688, heap 28690, total heap 3675809, stack 59
memory 30849776, heap 28690, total heap 3856070, stack 59
memory 32216544, heap 28690, total heap 4026916, stack 59


iex(16)> spawn(TTR, :factorial, [20_000])
#PID<0.136.0>
memory 2688, heap 233, total heap 233, stack 7
memory 55000, heap 6772, total heap 6772, stack 6007
memory 142672, heap 17731, total heap 17731, stack 12007
memory 230344, heap 28690, total heap 28690, stack 18007
memory 230344, heap 28690, total heap 28690, stack 24007
memory 372200, heap 46422, total heap 46422, stack 30007
memory 372200, heap 46422, total heap 46422, stack 36007
memory 372200, heap 46422, total heap 46422, stack 42007
memory 601728, heap 75113, total heap 75113, stack 48007
memory 601728, heap 75113, total heap 75113, stack 54007
memory 601728, heap 75113, total heap 75113, stack 60007
memory 601728, heap 75113, total heap 75113, stack 66007
memory 601728, heap 75113, total heap 75113, stack 72007
memory 973112, heap 121536, total heap 121536, stack 78007
memory 973112, heap 121536, total heap 121536, stack 84007
memory 973112, heap 121536, total heap 121536, stack 90007
memory 973112, heap 121536, total heap 121536, stack 96007
memory 973112, heap 121536, total heap 121536, stack 102007
memory 973112, heap 121536, total heap 121536, stack 108007
memory 973112, heap 121536, total heap 121536, stack 114007

iex(17)> spawn(TTR, :factorial_t, [20_000])
#PID<0.138.0>
memory 2688, heap 233, total heap 233, stack 8
memory 907040, heap 233, total heap 113277, stack 8
memory 2686696, heap 610, total heap 335734, stack 8
memory 12085240, heap 121536, total heap 1510552, stack 8
memory 13841864, heap 121536, total heap 1730130, stack 8
memory 15587544, heap 121536, total heap 1948340, stack 8
memory 17321528, heap 121536, total heap 2165088, stack 8
memory 19043112, heap 121536, total heap 2380286, stack 8
memory 20751312, heap 121536, total heap 2593811, stack 8
memory 22445040, heap 121536, total heap 2805527, stack 8
memory 24123064, heap 121536, total heap 3015280, stack 8
memory 25783880, heap 121536, total heap 3222882, stack 8
memory 27425672, heap 121536, total heap 3428106, stack 8
memory 29046264, heap 121536, total heap 3630680, stack 8
memory 30642456, heap 121536, total heap 3830204, stack 8
memory 32210896, heap 121536, total heap 4026259, stack 8
memory 33746272, heap 121536, total heap 4218181, stack 8
memory 35241072, heap 121536, total heap 4405031, stack 8
memory 36683160, heap 121536, total heap 4585292, stack 8
memory 38049928, heap 121536, total heap 4756138, stack 8

Итого, stack ведет себя как надо. heap одинаковый в обоих реализациях. Какая-то еще память растет с дикой скоростью, и в tail намного больше, чем в non-tail.
    
Реализация на эрланге памяти расходует меньше, но картина такая же.
```
22> spawn(factorial, factorial, [20000]).
<0.144.0>
[{memory,2688},{heap_size,233},{stack_size,3}]
[{memory,21512},{heap_size,2586},{stack_size,2003}]
[{memory,34304},{heap_size,4185},{stack_size,4003}]
[{memory,55000},{heap_size,6772},{stack_size,6003}]
[{memory,88488},{heap_size,10958},{stack_size,8003}]
[{memory,88488},{heap_size,10958},{stack_size,10003}]
[{memory,142672},{heap_size,17731},{stack_size,12003}]
[{memory,142672},{heap_size,17731},{stack_size,14003}]
[{memory,142672},{heap_size,17731},{stack_size,16003}]
[{memory,230344},{heap_size,28690},{stack_size,18003}]
[{memory,230344},{heap_size,28690},{stack_size,20003}]
[{memory,230344},{heap_size,28690},{stack_size,22003}]
[{memory,230344},{heap_size,28690},{stack_size,24003}]
[{memory,230344},{heap_size,28690},{stack_size,26003}]
[{memory,230344},{heap_size,28690},{stack_size,28003}]
[{memory,372200},{heap_size,46422},{stack_size,30003}]
[{memory,372200},{heap_size,46422},{stack_size,32003}]
[{memory,372200},{heap_size,46422},{stack_size,34003}]
[{memory,372200},{heap_size,46422},{stack_size,36003}]
[{memory,372200},{heap_size,46422},{stack_size,38003}]

24> spawn(factorial, factorial_t, [20000]).
<0.151.0>
[{memory,2688},{heap_size,233},{stack_size,4}]
[{memory,907040},{heap_size,233},{stack_size,4}]
[{memory,2686696},{heap_size,610},{stack_size,4}]
[{memory,12085240},{heap_size,121536},{stack_size,4}]
[{memory,13841864},{heap_size,121536},{stack_size,4}]
[{memory,15587544},{heap_size,121536},{stack_size,4}]
[{memory,17321528},{heap_size,121536},{stack_size,4}]
[{memory,19043112},{heap_size,121536},{stack_size,4}]
[{memory,20751312},{heap_size,121536},{stack_size,4}]
[{memory,22445040},{heap_size,121536},{stack_size,4}]
[{memory,24123064},{heap_size,121536},{stack_size,4}]
[{memory,25783880},{heap_size,121536},{stack_size,4}]
[{memory,27425672},{heap_size,121536},{stack_size,4}]
[{memory,29046264},{heap_size,121536},{stack_size,4}]
[{memory,30642456},{heap_size,121536},{stack_size,4}]
[{memory,32210896},{heap_size,121536},{stack_size,4}]
[{memory,33746272},{heap_size,121536},{stack_size,4}]
[{memory,35241072},{heap_size,121536},{stack_size,4}]
[{memory,36683160},{heap_size,121536},{stack_size,4}]
[{memory,38049928},{heap_size,121536},{stack_size,4}]
```

```
Non tail
[{memory,              783,792},
 {heap_size,            75,113},
 {total_heap_size,      97,871},
 {stack_size,           27,004},
 {total,            21,691,816},
 {processes,         5,393,912},
 {processes_used,    5,393,792},
 {system,           16,297,904},
 {atom,                434,361},
 {atom_used,           405,048},
 {binary,              359,112},
 {code,              7,198,291},
 {ets,                 389,408}]
 
Tail
 [{memory,          13,342,896},
  {heap_size,           10,958},
  {total_heap_size,  1,667,759},
  {stack_size,               5},
  {total,           26,550,256},
  {processes,       10,253,464},
  {processes_used,  10,252,192},
  {system,          16,296,792},
  {atom,               434,361},
  {atom_used,          405,048},
  {binary,             357,976},
  {code,             7,198,291},
  {ets,                389,408}]
```