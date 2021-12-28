defmodule Event do
  alias Model.TypedStruct, as: T
  alias Model.Record, as: R

  alias Model.Record.Participant, as: RP

  require R.Event
  require R.Address
  require R.Room
  require R.Location
  require R.Participant
  require R.Topic

  def sample_struct_event() do
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
      %T.Topic{subject: "Release WGM 2.0", description: "", priority: :high},
      %T.Topic{subject: "Buy food for cat", priority: :high, description: ""},
      %T.Topic{subject: "Backlog Refinement", priority: :low, description: ""}
    ]

    %T.Event{
      title: "Team Meeting",
      datetime: ~U[2021-12-16 16:00:00.000000Z],
      location: location,
      participants: participants,
      agenda: agenda
    }

  end

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
