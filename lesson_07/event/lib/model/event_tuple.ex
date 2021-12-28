defmodule Model.Tuple do

  defmodule Event do

    alias Model.Tuple.{Participant, Place, Topic}

    def example() do
      participants = [
        Participant.new(:human, "Helen", :developer),
        Participant.new(:human, "Bob", :developer),
        Participant.new(:human, "Kate", :project_manager),
        Participant.new(:cat, "Tihon", :cat)
      ]
      time = ~U[2021-03-12 15:30:00.000000Z] # Sigil
      place = Place.new()
      agenda = [
        Topic.new("release WGM 2.0", "disscuss release"),
        Topic.new("buy food for cat", "where to buy")
      ]
      {:event, participants, time, place, agenda}
    end

  end

  defmodule Participant do

    def new(type, name, role) do
      {type, name, role}
    end

  end

  defmodule Place do

    def new() do
      {:place, "Volna", "610c"}
    end

  end

  defmodule Topic do

    def new(subject, description) do
      {:topic, subject, description}
    end

  end

end
