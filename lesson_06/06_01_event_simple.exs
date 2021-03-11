datetime = ~U[2021-03-10 19:40:00.000000Z]

address = {:address, "Minsk", "Partizanskij pr", 178, 2}
room = {:room, 610}
location = {address, room}

participants = [
  {:user, "Helen", :project_manager},
  {:user, "Bob", :developer},
  {:user, "Kate", :developer},
  {:cat, "Tihon", :cat}
]

agenda = [
  {:topic, :high, "release my_cool_service v1.2.3"},
  {:topic, :medum, "buying food for cat"},
  {:topic, :low, "backlog refinement"}
]

event = {:event, "Team Meeting", datetime, location, participants, agenda}

event |> inspect |> IO.puts