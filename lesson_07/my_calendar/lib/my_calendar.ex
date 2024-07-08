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
end
