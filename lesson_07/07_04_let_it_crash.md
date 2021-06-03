# Defensive Programming vs Let It Crash

Когда вся программа выполняется в одном потоке, аварийное завершение
этого потока означает аварийное завершение программы. И если это
случилось в месте, где явно не предусмотрена обработка ошибок, то
остается минимум информации для диагностики проблемы.

Поэтому программисты стараются предусмотреть обработку всех возможных
ошибок во всех возможных местах. Такой стиль программирования
называется **Defensive Programming**. И он нередко приводит к тому,
что программа содержит больше кода для обработки ошибок, чем кода,
выполняющего основную задачу. Конечно, это усложняет и написание кода,
и поддержку.

Эликсир предлагает другой подход: реализовать только основную задачу
(**happy path**) и не писать код для обработки ошибок. Благодаря
многопоточности и разделению потоков на рабочие и супервизоры, любая
ошибка всегда будет замечена и записана в лог. А система в целом
продолжит работу. Этот подход называется **Let It Crash**.

Между тем, все инструменты для Defensive Programming в эрланг есть.
И полностью от этого подхода никто не отказывается.  На практике
каждый разработчик ищет свой баланс между Defensive Programming
и Let It Crash.

Almost all languages, including Elixir, have built-in mechanisms to handle
exceptions. These require that we identify risky code in advance, wrap it in
a block that tries to execute it, and provide a block to rescue the situation if
the code fails. In most languages, this kind of exception handling is essential,
but in Elixir we hardly ever have to reach for it.

it’s nearly impossible to predict all possible failures
in advance, so they decided to focus on recovering from failure instead.


Краш одного потока не влияет на остальные потоки (кроме супервизора)
Это отличается от некоторых других ЯП, где несколько потоков реализованы в рамках одного системного процесса, и краш одного потока крашит весь процесс и все потоки в нем
(TODO узнать подробнее об этом)

You won’t find much exception-handling code in Elixir programs.
Exceptions are raised, but you rarely catch them.

The Elixir source code for the mix utility contains no exception handlers.
The Elixir compiler itself contains a total of five
(but it is doing some pretty funky things).

If you find yourself defining new exceptions,
ask if you should be isolating the code in a separate process instead.
After all, if it can go wrong, wouldn’t you want to isolate it?
