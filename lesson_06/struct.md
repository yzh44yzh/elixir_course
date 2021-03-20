# Structs

TODO просмотреть, выбрать инфу:
https://elixir-lang.org/getting-started/structs.html

Struct is a special kind of map with a fixed structure.

Limitations:
- fixed set of fields
- the keys must be atoms
- don’t have Dict capabilities

The name of the module becomes the name of the map type.
```
defmodule Subscriber do
  defstruct name: "", paid: false, over_18: true
end
```

Access the fields in a struct using dot notation or pattern matching:
```
s3.name
%Subscriber{name: a_name} = s3
```

Update:
```
s4 = %Subscriber{ s3 | name: "Marie"}
```

Why are structs wrapped in a module? 
The idea is that you are likely to want to add struct-specific behavior.

Structs also play a large role when implementing polymorphism, 
which we’ll see when we look at protocols.

There is a tight relation between structs and modules. 
A struct can exists only in module, and a single module can define only one struct.

## Nested Structs

```
defmodule Customer do
  defstruct name: "", company: ""
end

defmodule BugReport do
  defstruct owner: %Customer{}, details: "", severity: 1
end

report = %BugReport{owner: %Customer{name: "Dave", company: "Pragmatic"}, details: "broken"}
# %BugReport{details: "broken", severity: 1, owner: %Customer{company: "Pragmatic", name: "Dave"}}

report.owner.company
# "Pragmatic"
```

Update:
```
report = %BugReport{ report | owner: %Customer{ report.owner | company: "PragProg" }}
```
This is verbose, hard to read, and error prone.
Fortunately, Elixir has a set of nested dictionary-access functions.
```
put_in(report.owner.company, "PragProg")
```
it’s simply a macro that generates the long-winded code we’d have to have written otherwise.


The update_in function lets us apply a function to a value in a structure.
```
update_in(report.owner.name, &("Mr. " <> &1))
```

The other two nested access functions are get_in and get_and_update_in.

Nested accessor functions work with maps or keyword lists.

Macroses have some limitations:
• The number of keys you pass a particular call is static.
• You can’t pass the set of keys as parameters between functions.

To overcome this, get_in, put_in, update_in, and get_and_update_in 
can all take a list of keys as a separate parameter. 
Adding this parameter changes them from macros to function calls, so they become dynamic.

_Нужны интересные примеры использования. Из книги не стал копировать, там длинные._

If you pass a function as a key, that function is invoked to return the corresponding values.
_Это уже какие-то дурацкие навороты._

The Access module provides a number of predefined functions to use as parameters to get and get_and_update_in.
_Еще больше этих наворотов. Проще использовать привычные map и filter._
