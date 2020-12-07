## Хвостовая рекурсия

В эрланг, как и во всех функциональных языках, нет циклов. Их
заменяет рекурсия.  Надо понимать, что рекурсия -- штука не
бесплатная. Каждый рекурсивный вызов требует сохранения на стеке
предыдущего состояния функции, чтобы вернуться в него после очередного
вызова.

И хотя стек в эрланге легковесный и позволяет делать миллионы рекурсивных
вызовов, (а не тысячи, как, в императивных языках), он, все-таки,
конечный.

Поэтому в эрланг, как и во многих других функциональных языках,
компилятор делает одну полезную штуку, которая называется
**оптимизация хвостовой рекурсии**.

Если рекурсивный вызов является последней строчкой кода в данной функции,
и после него больше никаких инструкций нет, то такой вызов называется хвостовым.
В этом случае нет необходимости возвращаться по стеку во все вызывающие
функции, а можно сразу отдать результат из последнего вызова.
И стек не растет, а используется заново каждым новым вызовом, и адрес
возврата в нем не меняется.

Это позволяет делать бесконечную рекурсию, которая нужна для
бесконечно живущих процессов.  А такие процессы нужны серверам :)


# Tail vs non-tail recursion

You might think that tail-call is always a preferred approach for doing loops. It's not that simple.

Non-tail recursion often looks more elegant and concise, and it can in some circumstances yield better performance.
(no need for accumulator)

If you need to run an infinite loop, tail recursion is the only way that will work.

If the last expression is a new function call, then the current function’s return is the return of the new function call and it doesn’t need to keep the current function in memory. 

We can simulate the memory problem that body-recursive function may face by using a big number to generate millions of recursive calls. You can use a process monitor to see the huge impact it will create in memory. Be ready to kill the process, because it will take a long time to finish. Try it in your IEx:​ ​iex>​ c(​"​​factorial.ex"​)​ ​iex>​ Factorial.of(10_000_000)

The common approach to creating tail-recursive functions is to replace the use of the function result with an extra argument that accumulates the results of each iteration. 

That’s a huge improvement! However, at the same time we made our code a bit more complex. It’s a serious trade-off that we need to think about before deciding if we should write a body-recursive or a tail-recursive function. In general, if you’re expecting millions of iterations or the tail-recursive function isn’t hard to read and maintain, go with tail-recursive. If the number of iterations is small and the tail-recursive function is hard to understand and maintain, go with body-recursive.


TODO примеры, как аккумуляторы помогают перейти от non-tail к tail
