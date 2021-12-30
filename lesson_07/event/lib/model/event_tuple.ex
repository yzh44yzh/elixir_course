defmodule Model.Tuple do

  defmodule Event do

    def new(title, participants, datetime, place, agenda) do
      {:event, title, participants, datetime, place, agenda}
    end

  end

  defmodule Participant do

    def new(type, name, role) do
      {type, name, role}
    end

  end

  defmodule Place do

    def new(office, room) do
      {:place, office, room}
    end

  end

  defmodule Topic do

    def new(subject, description) do
      {:topic, subject, description}
    end

  end

end
