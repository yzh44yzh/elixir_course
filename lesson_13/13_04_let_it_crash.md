# Let It Crash

еще раз про него

Defensive Programming -- try..catch
Let It Crash:
- Supervisor: 
  - restart process: lost state
  - endless restart: num restarts per time unit. 
- Clusters

“let it crash” or “fail fast”. And one of the most common ways we can recover from a failure is by restarting whatever part of the system crashed.

Once we restart the process, we reset the device back to its initial state, which is well-tested and guaranteed to work.

described in Joe Armstrong PhD thesis
"Making reliable distributed systems in the presence of software errors"

happy path programming, liberated from paranoid, defensive try/catch.
error handling is separated

There are 2 important situations in which you should explicitly handle an error:
- critical processes that shouldn't crash
- when you expect an error and can deal with it in a meaningful way


## Error Kernel pattern

Isolating the effect off errors allows other parts of system to run and provide service while you're recovering from error.

Error Kernel -- part of the system, that shouldn't crash. Critical for entire system to work.
Keep it small and simple.
If the code in your error-kernel process is complex, consider splitting it into two processes:
one that holds state (simple), and another that does the actual work (not error-kernel).
You can use defensive programming in error-kernel code.

Anyway you should always have a recovery plan for the crash of a critical process.

The whole point of the let-it-crash approach is to leave recovery of unexpected errors to supervisors.
But if you can predict an error and you have a way to deal with it -- do it.

It is a concrete representation of the idea of building rings of confidence in our code.
The outer ring, where our code interacts with the world, should be as reliable as we can make it.
But within that ring there are other, nested rings.
And in those rings, things can be less than perfect.
The trick is to ensure that the code in each ring knows how to deal with failures of the code in the next ring down.

Components with different
failure probability and
reliability requirements
are combined into a larger system or application.

Some functions of the system must never go down, whereas others are necessarily exposed to failure.

This pattern has been established in Erlang programs for decades.

The name Akka was originally conceived as a palindrome of Actor Kernel, referring to this core design pattern.

Сбой (fault) -- отклонение одного из компонентов системы от рабочих характеристик.

Отказ (failure) -- вся система в целом прекращает предоставление сервиса.


## State

State isn't preserved when a process is crashed. It could be inconsistent. That why it is important to start from clean state.
You can implement persistent state if you need. This isn't provided out of the box.
The general approach is to save the state outside of the process (another process or database),
then restore it when successor process is started.
Make sure you store consistent state. If you store inconsistent state restarts won't help you.
