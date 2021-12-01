# Связь Эликсир и Эрланг

TODO промежуточные представления, куда что компилируется

Erlang AST -> Core Erlang -> Kernel Erlang -> Beam SSA -> Beam Asm
TODO откуда эта инфа? Во что компилируется эликсир?

https://medium.com/intermediate-representation-toolchain-and/intermediate-representations-toolchain-and-internals-in-erlang-50d13ba61cad
IR Intermediate Representation
SSA (Static Single Assignment)

ERTS Erlang Runtime System

Программы компилируются в байт-код для виртуальной машины Erlang (BEAM)[8]. Каждый элемент программы является выражением[8], функции языка Erlang могут быть вызваны без влияния на время исполнения из-за компиляции байт-кода в Erlang и наоборот.

Erlang functions can be called from Elixir without run time impact, due to compilation to Erlang bytecode, and vice versa
