defmodule MyCalendar do

  def sample_event_tuple() do
    alias MyCalendar.Model.Tuple, as: T

    place = T.Place.new("Office #1", "Room 123")
    time = ~U[2024-07-05 15:00:00Z]
    participants = [
      T.Participant.new("Kate", :project_manager),
      T.Participant.new("Bob", :developer),
      T.Participant.new("Bill", :qa),
    ]
    agenda = [
      T.Topic.new("Release MyCalendar 1.0", "disscuss release"),
      T.Topic.new("Buy cookies", "disscuss cookies")
    ]
    T.Event.new("Team Meeting", place, time, participants, agenda)
  end

  def sample_event_map() do
    alias MyCalendar.Model.Map, as: M

    place = M.Place.new("Office #1", "Room 123")
    time = ~U[2024-07-05 15:00:00Z]
    participants = [
      M.Participant.new("Kate", :project_manager),
      M.Participant.new("Bob", :developer),
      M.Participant.new("Bill", :qa),
    ]
    agenda = [
      M.Topic.new("Release MyCalendar 1.0", "disscuss release"),
      M.Topic.new("Buy cookies", "disscuss cookies")
    ]
    M.Event.new("Team Meeting", place, time, participants, agenda)
  end

  def sample_event_struct() do
    alias MyCalendar.Model.Struct, as: S

    place = %S.Place{office: "Office #1", room: "Room 123"}
    time = ~U[2024-07-05 15:00:00Z]
    participants = [
      %S.Participant{name: "Kate", role: :project_manager},
      %S.Participant{name: "Bob", role: :developer},
      %S.Participant{name: "Bill", role: :qa}
    ]
    agenda = [
      %S.Topic{subject: "Release MyCalendar 1.0", description: "disscuss release"},
      %S.Topic{subject: "Buy cookies", description: "disscuss cookies", priority: :low}
    ]
    %S.Event{
      title: "Team Meeting",
      place: place,
      time: time,
      participants: participants,
      agenda: agenda
    }
  end
end
