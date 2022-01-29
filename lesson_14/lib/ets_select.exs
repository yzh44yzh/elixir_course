defmodule Lesson_14 do

  defmodule UserS do
    defstruct [:id, :name, :age]
  end

  defmodule UserR do
    require Record
    
    Record.defrecord(:user,
      id: nil,
      name: nil,
      age: nil
    )
  end

  defmodule SelectData do
    
    alias Lesson_14.UserS
    alias Lesson_14.UserR
    require UserR
    
    def data_as_tuple() do
      [
        {:user, 1, "Bob", 42},
        {:user, 2, "Helen", 20},
        {:user, 3, "Bill", 18},
        {:user, 4, "Kate", 15}
      ]
    end

    def data_as_map() do
      [
        %{id: 1, name: "Bob", age: 42},
        %{id: 2, name: "Helen", age: 20},
        %{id: 3, name: "Bill", age: 18},
        %{id: 4, name: "Kate", age: 15}
      ]
    end

    def data_as_struct() do
      [
        %UserS{id: 1, name: "Bob", age: 42},
        %UserS{id: 2, name: "Helen", age: 20},
        %UserS{id: 3, name: "Bill", age: 18},
        %UserS{id: 4, name: "Kate", age: 15}
      ]
    end

    def data_as_record() do
      [
        UserR.user(id: 1, name: "Bob", age: 42),
        UserR.user(id: 2, name: "Helen", age: 20),
        UserR.user(id: 3, name: "Bill", age: 18),
        UserR.user(id: 4, name: "Kate", age: 15)
      ]
    end

  end
  
end
