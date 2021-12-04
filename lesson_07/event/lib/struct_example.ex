defmodule StructExample do

  alias Model.Event

  def create() do
    address = %Event.Address{city: "Minsk", street: "Partizanskij pr", house_number: 178}
    room = %Event.Room{number: 610}
    location = %Event.Location{address: address, room: room}

    helen = %Event.Participant{name: "Helen", role: :project_manager}
    bob = %Event.Participant{name: "Bob", role: :developer}
    kate = %Event.Participant{name: "Kate", role: :developer}
    tihon = %Event.Participant{species: :cat, name: "Tihon", role: :cat}

    agenda = [
      %Event.Topic{title: "release my_cool_service v1.2.3", priority: :high},
      %Event.Topic{title: "buying food for cat"},
      %Event.Topic{title: "backlog refinement", priority: :low}
    ]

    %Event.Event{
      title: "Team Meeting",
      datetime: ~U[2021-03-10 19:40:00.000000Z],
      location: location,
      participants: [helen, bob, kate, tihon],
      agenda: agenda
    }
  end

end