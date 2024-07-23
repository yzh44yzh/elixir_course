defmodule MyCalendar.Model.Tuple do
  defmodule Place do
    def new(office, room) do
      {:place, office, room}
    end
  end

  defmodule Participant do
    def new(name, role) do
      {:participant, name, role}
    end
  end

  defmodule Topic do
    def new(subject, description) do
      {:topic, subject, description}
    end
  end

  defmodule Event do
    def new(title, place, time, participants, agenda) do
      {:event, title, place, time, participants, agenda}
    end
  end
end
