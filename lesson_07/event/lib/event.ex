defmodule Event do
  
  alias Model.Tuple, as: T

  def sample_tuple_event() do
      participants = [
        T.Participant.new(:human, "Helen", :developer),
        T.Participant.new(:human, "Bob", :developer),
        T.Participant.new(:human, "Kate", :project_manager),
        T.Participant.new(:cat, "Tihon", :cat)
      ]
      datetime = ~U[2021-03-12 15:30:00.000000Z] 
      place = T.Place.new("Volna", "610c")
      agenda = [
        T.Topic.new("Release CoolProject 2.0", "disscuss release"),
        T.Topic.new("Buy food for cat", "where to buy")
      ]
      T.Event.new("Team Meeting", participants, datetime, place, agenda)
  end

  alias Model.Map, as: M

  def sample_map_event() do
    participants = [
      M.Participant.new(:human, "Helen", :developer),
      M.Participant.new(:human, "Bob", :developer),
      M.Participant.new(:human, "Kate", :project_manager),
      M.Participant.new(:cat, "Tihon", :cat)
    ]
    datetime = ~U[2021-03-12 15:30:00.000000Z] 
    place = M.Place.new("Volna", "610c")
    agenda = [
      M.Topic.new("Release CoolProject 2.0", "disscuss release"),
      M.Topic.new("Buy food for cat", "where to buy")
    ]
    M.Event.new("Team Meeting", participants, datetime, place, agenda)
  end  
  
  alias Model.Struct, as: S

  def sample_struct_event() do
    address = %S.Address{
      country: "Belarus",
      city: "Minsk",
      street: "Partizanskij prt",
      building: 178
    }
    room = %S.Room{floor: 6, number: 610}
    location = %S.Location{address: address, room: room}

    participants = [
      %S.Participant{name: "Helen", role: :developer},
      %S.Participant{name: "Bob", role: :developer},
      %S.Participant{name: "Kate", role: :developer},
      %S.Participant{species: :cat, name: "Tihon", role: :cat}
    ]

    agenda = [
      %S.Topic{subject: "Release CoolProject 2.0", description: "disscuss release", priority: :high},
      %S.Topic{subject: "Buy food for cat", priority: :high, description: "where to buy"},
      %S.Topic{subject: "Backlog Refinement", priority: :low, description: ""}
    ]

    %S.Event{
      title: "Team Meeting",
      datetime: ~U[2021-12-16 16:00:00.000000Z],
      location: location,
      participants: participants,
      agenda: agenda
    }
  end

  alias Model.TypedStruct, as: T

  def sample_typed_struct_event() do
    address = %T.Address{
      country: "Belarus",
      city: "Minsk",
      street: "Partizanskij prt",
      building: 178
    }
    room = %T.Room{floor: 6, number: 610}
    location = %T.Location{address: address, room: room}

    participants = [
      %T.Participant{name: "Helen", role: :developer},
      %T.Participant{name: "Bob", role: :developer},
      %T.Participant{name: "Kate", role: :developer},
      %T.Participant{species: :cat, name: "Tihon", role: :cat}
    ]

    agenda = [
      %T.Topic{subject: "Release WGM 2.0", description: "disscuss release", priority: :high},
      %T.Topic{subject: "Buy food for cat", priority: :high, description: "where to buy"},
      %T.Topic{subject: "Backlog Refinement", priority: :low}
    ]

    %T.Event{
      title: "Team Meeting",
      datetime: ~U[2021-12-16 16:00:00.000000Z],
      location: location,
      participants: participants,
      agenda: agenda
    }
  end

  alias Model.Record, as: R
  alias Model.Record.Participant, as: RP

  require R.Event
  require R.Address
  require R.Room
  require R.Location
  require R.Participant
  require R.Topic

  def sample_record_event() do
    address = R.Address.address(city: "Minsk", street: "Partizanskij pr", building: 178)
    room = R.Room.room(floor: 6, number: 610)
    location = R.Location.location(address: address, room: room)

    helen = RP.participant(name: "Helen", role: :project_manager)
    bob = RP.participant(name: "Bob", role: :developer)
    kate = RP.participant(name: "Kate", role: :developer)
    tihon = RP.participant(name: "Tihon", role: :cate, species: :cat)

    agenda = [
      R.Topic.topic(subject: "Release WGM 2.0", priority: :high),
      R.Topic.topic(subject: "Buy food for cat", priority: :high),
      R.Topic.topic(subject: "Backlog Refinement", priority: :low)
    ]

    R.Event.event(title: "Team Meeting",
      datetime: ~U[2021-12-06 16:00:00.000000Z],
      location: location,
      participants: [helen, bob, kate, tihon],
      agenda: agenda
    )
  end

  def show_size(term) do
    size = :erts_debug.flat_size(term) * 8
    str_term = inspect(term, limit: 2, printable_limit: 20)
    IO.puts("#{size} bytes for #{str_term}")
  end

  def modify_event(event) do
    # put_in(event.location.room.number, 100)
    update_in(event.location.room.number,
      fn(number) -> number + 100 end)
  end

  def modify_map_event(event) do
    put_in(event, [:place, :room], "100")
    update_in(event, [:place, :room], fn(number) -> number <> "C" end)
  end


end
