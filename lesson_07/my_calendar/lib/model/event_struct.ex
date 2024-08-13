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
      # :high | :medium | :low
      {:priority, :medium},
      :description
    ]
  end

  defmodule Event do
    # alias MyCalendar.Model.CalendarItem

    @enforce_keys [:title, :place, :time, :participants, :agenda]
    defstruct [:title, :place, :time, :participants, :agenda]

    def add_participant(
          %Event{participants: participants} = event,
          %Participant{} = participant
        ) do
      %Event{event | participants: [participant | participants]}
    end

    def replace_participant(
          %Event{participants: participants} = event,
          %Participant{} = updated_participant
        ) do
      participants =
        Enum.filter(participants, fn p ->
          p.name != updated_participant.name
        end)

      %Event{event | participants: [updated_participant | participants]}
    end

    # defimpl CalendarItem do

    #   @spec get_title(CalendarItem.t()) :: String.t()
    #   def get_title(event), do: event.title

    #   @spec get_time(CalendarItem.t()) :: DateTime.t()
    #   def get_time(event), do: event.time
    # end
  end
end
