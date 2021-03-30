
TODO просмотреть, выбрать инфу:
https://elixir-lang.org/getting-started/protocols.html

```
alias Model.Calendar, as: C
c = C.new

e1 = TypedStructExample.create
e2 = StructExample.create
e3 = SimpleExample.create_map
c = C.add_item(c, e1)
c = C.add_item(c, e2)
c = C.add_item(c, e3)
C.show(c)

e4 = SimpleExample.create
c = C.add_item(c, e4)
C.show(c)

** (Protocol.UndefinedError) protocol Model.CalendarItem not implemented for {:event, ...
```

```
mix dialyzer
```
dialyzir 1.1.0 -- все ок. В более старой версии было не ок