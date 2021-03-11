defmodule Lesson_06.Task_06_01_ADT do

  _ = """
  datetime = ~U[2021-03-10 19:40:00.000000Z]

  participants = [
  {:user, "Helen", :project_manager},
  {:user, "Bob", :developer},
  {:user, "Kate", :developer},
  {:cat, "Tihon", :cat}
  ]

  agenda = [
  {:topic, :high, "release my_cool_service v1.2.3"},
  {:topic, :medum, "buying food for cat"},
  {:topic, :low, "backlog refinement"}
  ]

  event = {:event, "Team Meeting", datetime, place, participants, agenda}
  """

  defmodule Address do
    @enforce_keys [:city, :street, :house_number]

    defstruct [
      {:country, "Belarus"},
      :city,
      :street,
      :house_number
    ]
  end

  defmodule Room do
    @enforce_keys [:number]
    defstruct [:floor, :number]
  end

  defmodule Location do
    @enforce_keys [:address, :room]
    defstruct [:address, :room]
  end

end

