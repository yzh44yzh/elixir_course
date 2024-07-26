defmodule MyCalendar.Model.TypedStruct do
  defmodule Place do
    @type t() :: %Place{
            office: String.t(),
            room: String.t()
          }

    @enforce_keys [:office, :room]

    defstruct [:office, :room]
  end

  defmodule Participant do
    @type t() :: %__MODULE__{
            name: String.t(),
            role: atom()
          }

    @enforce_keys [:name]

    defstruct [:name, :role]
  end

  defmodule Topic do
    @type t() :: %__MODULE__{
            subject: String.t(),
            priority: :high | :medium | :low,
            description: String.t()
          }

    @enforce_keys [:subject]

    defstruct [
      :subject,
      {:priority, :medium},
      :description
    ]
  end

  defmodule Event do
    @type t() :: %__MODULE__{
            title: String.t(),
            place: Place.t(),
            time: DateTime.t(),
            participants: [Participant.t()],
            agenda: [Topic.t()]
          }

    @enforce_keys [:title, :place, :time, :participants, :agenda]

    defstruct [:title, :place, :time, :participants, :agenda]
  end
end
