alias Lesson_06.Task_06_01_ADT.{Address, Room, Location}

address = %Address{city: "Minsk", street: "Partizanskij pr", house_number: 178}
room = %Room{number: 610}
location = %Location{address: address, room: room}

location |> inspect |> IO.puts
