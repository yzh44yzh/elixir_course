alias Lesson_06.Task_06_02_TypedEvent, as: TE

address = %TE.Address{city: "Minsk", street: "Partizanskij pr", house_number: 178}
room = %TE.Room{number: 610}
location = %TE.Location{address: address, room: room}

helen = %TE.Participant{name: "Helen", role: :project_manager}
bob = %TE.Participant{name: "Bob", role: :developer}
kate = %TE.Participant{name: "Kate", role: :developer}
tihon = %TE.Participant{species: :cat, name: "Tihon", role: :cat}

agenda = [
  %TE.Topic{title: "release my_cool_service v1.2.3", priority: :high},
  %TE.Topic{title: "buying food for cat"},
  %TE.Topic{title: "backlog refinement", priority: :low}
]

event = %TE.Event{
  title: "Team Meeting",
  datetime: ~U[2021-03-10 19:40:00.000000Z],
  location: location,
  participants: [helen, bob, kate, tihon],
  agenda: agenda
}

event |> inspect |> IO.puts
