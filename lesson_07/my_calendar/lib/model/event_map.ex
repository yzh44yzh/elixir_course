defmodule MyCalendar.Model.Map do

  defmodule Event do

    def new(title, participants, datetime, place, agenda) do
      %{
        title: title,
        type: :event,
        participant: participants,
        datetime: datetime,
        place: place,
        agenda: agenda
      }
    end

  end

  defmodule Participant do

    def new(type, name, role) do
      %{
        type: type,
        name: name,
        role: role
      }
    end

  end

  defmodule Place do

    def new(office, room) do
      %{
        type: :place,
        office: office,
        room: room
      }
    end

  end

  defmodule Topic do

    def new(subject, description) do
      %{
        type: :topic,
        subject: subject,
        description: description
      }
    end

  end

end
