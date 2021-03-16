https://hexdocs.pm/elixir/Record.html

Struct vs Record внешне одинаковые, разница во внутреннем представлении: map vs tuple, соотвественно, в расходе памяти.
в Эликсире выбрали Struct, в Эрланге Record.  

historical, used before maps

Record -- более естественное представление ADT, чем Struct. 

Активно используются в Эрланг, поэтому могут понадобится при использовании эрланговских библиотек.