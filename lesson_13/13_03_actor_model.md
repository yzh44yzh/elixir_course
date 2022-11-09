# Модели многопоточности

Seven Concurrency Models In Seven Weeks
Paul Butcher

1. Thread and Locks. Shared memory. 
2. Immutability. FP. 
3. Closure. STM. Software Transactional Memory. in-memory database, ACI.
4. CSP. Communicating Sequential Processes. Go lang. Channel - thread-safe queue. in-memory MB
5. Actor Model. Java,Scala, .NET.
6. Data Parallelism. 
7. Lambda Arch

Многопоточность, паралелизм. 

Concurrency -- many tasks at the same time.
many task / one or many workers
one-process concurrency: JavaScript, Python, Ruby.

Parallelism -- many workers (process, thread).
many workers / one or many tasks


## ActorModel

https://en.wikipedia.org/wiki/Actor_model
https://ru.wikipedia.org/wiki/%D0%9C%D0%BE%D0%B4%D0%B5%D0%BB%D1%8C_%D0%B0%D0%BA%D1%82%D0%BE%D1%80%D0%BE%D0%B2

Actor (GenServer): state, send/receive msgs. 
Joe Armstrong. Actors -- People.

GenServer имеет некоторое сходство с ООП:
- у него есть внутренее состояние, скрытое от внешних пользователей;
- у него есть публичное АПИ;
- для одного модуля можно запустить несколько процессов, и каждый процесс будет иметь свою собственную копию состояния;


## Другие модели тоже присутствуют в каком-то виде

3. STM. Software Transactional Memory -- Mnesia
4. CSP. Communicating Sequential Processes. -- RabbitMQ
6. Data Parallelism
7. Lambda Arch
