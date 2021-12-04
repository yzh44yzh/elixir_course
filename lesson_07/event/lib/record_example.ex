defmodule RecordExample do

  alias Model.RecordEvent, as: RE
  import RE.Address
  import RE.Room
  import RE.Location
  import RE.Participant
  import RE.Topic
  import RE.Event

  def create() do
    address = address(city: "Minsk", street: "Partizanskij pr", house_number: 178)
    room = room(floor: 6, number: 610)
    location = location(address: address, room: room)

    helen = participant(name: "Helen", role: :project_manager)
    tihon = participant(name: "Tihon", role: :cate, species: :cat)

    agenda = [
      topic(title: "buying food for cat")
    ]

    event(title: "Team Meeting",
      datetime: ~U[2021-03-10 19:40:00.000000Z],
      location: location,
      participants: [helen, tihon],
      agenda: agenda
    )
  end

end
