defmodule CompareStructRecord do

  def start() do
    show_size(TypedStructExample.create())
    show_size(RecordExample.create())
    show_size(%{a: 42, b: 1000})
    show_size({42, 1000})
    show_size([42, 1000])

    Enum.map(0..1000, fn(_) -> TypedStructExample.create() end) |> show_size()
    Enum.map(0..1000, fn(_) -> RecordExample.create() end) |> show_size()
  end

  def show_size(term) do
    str_term = inspect(term, limit: 2, printable_limit: 20)
    size = :erts_debug.flat_size(term) * 8
    IO.puts("#{size} bytes for #{str_term}")
  end

end
