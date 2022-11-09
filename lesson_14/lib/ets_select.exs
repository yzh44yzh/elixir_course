defmodule SelectData do

  defmodule User do
    defstruct [:id, :name, :age]
  end
  
  def data_as_tuple() do
    [
      {:user, 1, "Bob", 42},
      {:user, 2, "Helen", 20},
      {:user, 3, "Bill", 18},
      {:user, 4, "Kate", 15}
    ]
  end

  def select_tuple() do
    tid = :ets.new(:my_table, keypos: 2)
    :ets.insert(tid, data_as_tuple())

    IO.puts("lookup")
    Enum.each(1..6, fn(id) ->
      :ets.lookup(tid, id) |> inspect() |> IO.puts()
    end)

    IO.puts("match")
    :ets.match(tid, {:user, :"$1", :"$2", :_}) |> inspect() |> IO.puts()
    
    IO.puts("match 18")
    :ets.match(tid, {:user, :"$1", :"$2", 18}) |> inspect() |> IO.puts()

    IO.puts("match object")
    :ets.match_object(tid, {:user, :_, "Kate", :_}) |> inspect() |> IO.puts()

    IO.puts("select")
    # works in iex shell, doesn't work in compiled code:
    # match_spec = :ets.fun2ms(fn({:user, id, name, age}) when age >= 20 -> {id, name} end)
    # deps: {:ex2ms, "~> 1.6"}

    match_spec = [
      {
        {:user, :"$1", :"$2", :"$3"},
        [{:>=, :"$3", 20}],
        [{{:"$1", :"$2"}}]}
    ]
    :ets.select(tid, match_spec) |> inspect() |> IO.puts()
  end

  def data_as_map() do
    [
      %{id: 1, name: "Bob", age: 42},
      %{id: 2, name: "Helen", age: 20},
      %{id: 3, name: "Bill", age: 18},
      %{id: 4, name: "Kate", age: 15}
    ]
  end
  
  def select_map() do
    tid = :ets.new(:my_table, [])
    Enum.each(data_as_map(),
      fn(%{id: id} = item) ->
        :ets.insert(tid, {id, item})
      end)

    IO.puts("lookup")
    Enum.each(1..6, fn(id) ->
      :ets.lookup(tid, id) |> inspect() |> IO.puts()
    end)
    
    IO.puts("match")
    :ets.match_object(tid, {2, :_}) |> inspect() |> IO.puts()

    IO.puts("select")
    # match_spec = :ets.fun2ms(fn({id, item}) when item.age >= 20 -> item end)
    match_spec = [{{:"$1", :"$2"}, [{:>=, {:map_get, :age, :"$2"}, 20}], [:"$2"]}]
    :ets.select(tid, match_spec) |> inspect() |> IO.puts()
  end

  def data_as_struct() do
    [
      %User{id: 1, name: "Bob", age: 42},
      %User{id: 2, name: "Helen", age: 20},
      %User{id: 3, name: "Bill", age: 18},
      %User{id: 4, name: "Kate", age: 15}
    ]
  end

  def select_struct() do
    tid = :ets.new(:my_table, [])
    Enum.each(data_as_struct(),
      fn(item) ->
        :ets.insert(tid, {item.id, item})
      end)

    IO.puts("lookup")
    Enum.each(1..6, fn(id) ->
      :ets.lookup(tid, id) |> inspect() |> IO.puts()
    end)
    
    IO.puts("match")
    :ets.match_object(tid, {3, :_}) |> inspect() |> IO.puts()

    IO.puts("select")
    # match_spec = :ets.fun2ms(fn({id, item}) when item.age >= 20 -> item end)
    match_spec = [{{:"$1", :"$2"}, [{:>=, {:map_get, :age, :"$2"}, 20}], [:"$2"]}]
    :ets.select(tid, match_spec) |> inspect() |> IO.puts()
  end

end
