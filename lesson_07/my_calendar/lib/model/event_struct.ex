defmodule MyCalendar.Model.Struct do

  defmodule Place do
    @behaviour Access
    @enforce_keys [:office, :room]
    defstruct [:office, :room]

    @impl true
    def fetch(%Place{} = place, :office), do: {:ok, place.office}
    def fetch(%Place{} = place, :room), do: {:ok, place.room}
    def fetch(_, _), do: :error

    @impl true
    def get_and_update(%Place{} = place, :office, f) do
      {curr_val, new_val} = f.(place.office)
      new_place = %Place{place | office: new_val}
      {curr_val, new_place}
    end

    def get_and_update(place, _key, _f), do: {nil, place}

    @impl true
    def pop(place, _key), do: {nil, place}
  end

  defmodule Participant do
    @enforce_keys [:name]
    defstruct [:name, :role]
  end

  defmodule Topic do
    @enforce_keys [:subject]
    defstruct [
      :subject,
      {:priority, :medium}, # :high | :medium | :low
      :description
    ]
  end

  defmodule Event do
    @enforce_keys [:title, :place, :time, :participants, :agenda]
    defstruct [:title, :place, :time, :participants, :agenda]

    def add_participant(
          %Event{participants: participants} = event,
          %Participant{} = participant
        ) do
      %Event{event | participants: [participant | participants]}
    end
  end

end
