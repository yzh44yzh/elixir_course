Figure 1-3 illustrates a typical release of an Erlang node with the virtual machine (VM) dependent on the hardware and operating system, and Erlang applications running on top of the VM interfacing with non-Erlang components that are OS and hardware dependent.

lesson_11/img/otp.png -- сделать свою версию этой картинки

Не одна картинка, а несколько. От малого к большому:

+ Erlang Virutal Machine:
  - ERTS: планировщик, управление памятью, IO (сеть, диск), криптография
  - BEAM: компилятор, интерпретатор
  - OTP Apps: kernel, stdlib, inets, observer, crypto
  - My Apps: my_cool_app, my_other_app
  - Dependencies: elixir, iex, mix, logger, cowboy, plug, jason, prometheus.

- Host (железный или виртуальный или контейнер):
  - Erlang VM Node: одна или больше нод (типично сочетать с rabbitmq),
  - взаимодействие с другими процессами ОС (через порты).

- кластер: ноды на разных хостах в одном ДЦ

- федерация: кластеры из разных ДЦ.
