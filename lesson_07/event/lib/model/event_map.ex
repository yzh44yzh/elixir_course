defmodule Model.Map do

  defmodule Event do

    alias Model.Map.{Participant, Place, Topic}

    @type t :: %{atom => any}

    def example() do
      participants = [
        Participant.new(:human, "Helen", :developer),
        Participant.new(:human, "Bob", :developer),
        Participant.new(:human, "Kate", :project_manager),
        Participant.new(:cat, "Tihon", :cat)
      ]
      datetime = ~U[2021-03-12 15:30:00.000000Z] # Sigil
      place = Place.new()
      agenda = [
        Topic.new("release WGM 2.0", "disscuss release"),
        Topic.new("buy food for cat", "where to buy")
      ]
      %{
        title: "Some Event",
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

    def new() do
      %{
        type: :place,
        office: "Volna",
        room: "610c"
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
