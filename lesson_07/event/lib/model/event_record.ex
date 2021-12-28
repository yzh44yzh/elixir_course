defmodule Model.Record do

  require Record

  defmodule Address do
    @type t :: record(:address,
      country: String.t(),
      city: String.t(),
      street: String.t(),
      building: integer
    )
    Record.defrecord(:address,
      country: "Belarus",
      city: nil,
      street: nil,
      building: nil
    )
  end

  defmodule Room do
    @type t :: record(:room,
      floor: integer,
      number: integer
    )
    Record.defrecord(:room,
      floor: nil,
      number: nil
    )
  end

  defmodule Location do
    @type t :: record(:location,
      address: Address.t,
      room: Room.t
    )
    Record.defrecord(:location,
      address: nil,
      room: nil
    )
  end

  defmodule Participant do
    @type t :: record(:participant,
      species: atom,
      name: String.t,
      role: atom
    )
    Record.defrecord(:participant,
      species: :human,
      name: nil,
      role: nil
    )
  end

  defmodule Topic do
    @type t :: record(:topic,
      subject: String.t,
      priority: :high | :medium | :low,
      description: String.t
    )
    Record.defrecord(:topic,
      subject: nil,
      priority: :medium,
      description: nil
    )
  end

  defmodule Event do
    @type t :: record(:event,
      title: String.t,
      datetime: DateTime.t,
      location: Location.t,
      participants: list(Partcipant.t),
      agenda: list(Topic.t)
    )
    Record.defrecord(:event,
      title: nil,
      datetime: nil,
      location: nil,
      participants: [],
      agenda: []
    )
  end

end
