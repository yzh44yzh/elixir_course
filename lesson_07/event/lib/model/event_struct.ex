defmodule Model.Struct do

  defmodule Event do
    @enforce_keys [:title, :datetime, :location, :participants, :agenda]
    defstruct [:title, :datetime, :location, :participants, :agenda]
  end

  defmodule Participant do
    @enforce_keys [:name]
    defstruct [
      {:species, :human},
      :name,
      :role
    ]
  end

  defmodule Location do
    @enforce_keys [:address, :room]
    defstruct [
      :address,
      :room
    ]
  end

  defmodule Address do
    @enforce_keys [:country, :city, :street, :building]
    defstruct [
      :country,
      :city,
      :street,
      :building
    ]
  end

  defmodule Room do
    defstruct [
      floor: 1,
      number: 0
    ]
  end

  defmodule Topic do
    @enforce_keys [:subject]
    defstruct [
      :subject,
      {:priority, :medium}, # :high | :medium | :low
      :description
    ]

  end

end

defimpl Model.CalendarItem, for: Model.Struct.Event do

  def get_title(event) do
    event.title
  end

  def get_datetime(event) do
    event.datetime
  end

end
