defmodule Lesson_06.Task_06_01_Event do

  defmodule Event do
    defstruct [
      :title,
      :datetime,
      :location,
      :participants,
      :agenda
    ]
  end

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

  defmodule Participant do
    @enforce_keys [:name, :role]
    # syntax sugar to define default values:
    defstruct [
      species: :human,
      name: nil,
      role: nil
    ]
  end

  defmodule Topic do
    @enforce_keys [:title]
    defstruct [
      {:priority, :medium},
      :title
    ]
  end

end

