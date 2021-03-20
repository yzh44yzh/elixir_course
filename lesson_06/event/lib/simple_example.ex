defmodule SimpleExample do

  def create() do
    datetime = ~U[2021-03-10 19:40:00.000000Z]

    address = {:address, "Minsk", "Partizanskij pr", 178, 2}
    room = {:room, 610}
    location = {address, room}

    participants = [
      {:human, "Helen", :project_manager},
      {:human, "Bob", :developer},
      {:human, "Kate", :developer},
      {:cat, "Tihon", :cat}
    ]

    agenda = [
      {:topic, :high, "release my_cool_service v1.2.3"},
      {:topic, :medium, "buying food for cat"},
      {:topic, :low, "backlog refinement"}
    ]

    {:event, "Team Meeting", datetime, location, participants, agenda}
  end

  def create_map() do
    %{
      title: "Team Meeting",
      datetime: ~U[2021-03-10 19:40:00.000000Z],
      location: %{
        address: %{
          city: "Minsk",
          street: "Partizanskij pr",
          house_number: 178
        },
        room: %{
          number: 610,
          floor: 6
        }
      },
      participants: [
        %{
          name: "Helen",
          role: :project_manager,
          species: :human
        },
        %{
          name: "Bob",
          role: :developer,
          species: :human
        },
        %{
          name: "Kate",
          role: :developer,
          species: :human
        },
        %{
          name: "Tihon",
          role: :cat,
          species: :cat
        }
      ],
      agenda: [
        %{
          title: "release my_cool_service v1.2.3",
          priority: :high
        },
        %{
          title: "buying food for cat",
          priority: :medium
        },
        %{
          title: "backlog refinement",
          priority: :low
        }
      ]
    }
  end

end