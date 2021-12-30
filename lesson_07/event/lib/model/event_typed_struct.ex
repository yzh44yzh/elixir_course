defmodule Model.TypedStruct do

  defmodule Event do
    alias Model.TypedStruct.Topic
    @type t :: %__MODULE__{
      title: String.t,
      datetime: DateTime.t,
      location: Model.TypedStruct.Location.t,
      participants: [Model.TypedStruct.Participant.t],
      agenda: [Topic.t]
    }
    @enforce_keys [:title, :datetime, :location, :participants, :agenda]
    defstruct [:title, :datetime, :location, :participants, :agenda]

    defimpl Model.CalendarItem do

      @spec get_title(CalendarItem.t) :: String.t
      def get_title(event) do
         event.title
      end

      @spec get_datetime(CalendarItem.t) :: DateTime.t
      def get_datetime(event) do
        event.datetime
      end

    end

  end

  defmodule Participant do
    @type t :: %__MODULE__{
      species: :human | :cat,
      name: String.t,
      role: :developer | :project_manager | :qa | :ba | :cat
    }
    @enforce_keys [:name]
    defstruct [
      {:species, :human},
      :name,
      :role
    ]
  end

  defmodule Location do
    alias Model.TypedStruct.{Address, Room}
    @type t :: %__MODULE__{
      address: Address.t,
      room: Room.t
    }
    @enforce_keys [:address, :room]
    defstruct [
      :address,
      :room
    ]
  end

  defmodule Address do
    @type t :: %__MODULE__{
      country: String.t,
      city: String.t,
      street: String.t,
      building: integer
    }
    @enforce_keys [:country, :city, :street, :building]
    defstruct [
      :country,
      :city,
      :street,
      :building
    ]
  end

  defmodule Room do
    @type t :: %__MODULE__{
      floor: integer,
      number: integer
    }
    defstruct [
      floor: 1,
      number: 0
    ]
  end

  defmodule Topic do
    @type t :: %__MODULE__{
      subject: String.t,
      priority: :high | :medium | :low,
      description: String.t
    }

    @enforce_keys [:subject]
    defstruct [
      :subject,
      {:priority, :medium},
      {:description, ""}
    ]
  end

end
