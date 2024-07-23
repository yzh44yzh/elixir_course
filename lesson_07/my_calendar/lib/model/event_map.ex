defmodule MyCalendar.Model.Map do
  defmodule Place do
    def new(office, room) do
      %{
        office: office,
        room: room
      }
    end
  end

  defmodule Participant do
    def new(name, role) do
      %{
        name: name,
        role: role
      }
    end
  end

  defmodule Topic do
    def new(subject, description) do
      %{
        subject: subject,
        description: description
      }
    end
  end

  defmodule Event do
    def new(title, place, time, participants, agenda) do
      %{
        title: title,
        place: place,
        time: time,
        participants: participants,
        agenda: agenda
      }
    end

    def add_participant(event, participant) do
      Map.update!(event, :participants, fn p -> [participant | p] end)
    end
  end
end
