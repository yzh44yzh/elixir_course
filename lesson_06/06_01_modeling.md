# Моделирование предметной области

Давайте смоделируем такой достаточно сложный объект, как "встреча" (митинг, совещание).

Митинг состоит из:
- место
- время
- участники
- агенда

TODO нарисовать схему

В принципе, для моделирования сущностей любой сложности достаточно кортежей и списков. И в нашем случае это может выглядеть так:

event_simple.exs:
```
iex(11)> event
{:event, "Team Meeting", ~U[2021-03-10 19:40:00.000000Z],
 {{:address, "Minsk", "Partizanskij pr", 178, 2}, {:room, 610}},
 [
   {:human, "Helen", :project_manager},
   {:human, "Bob", :developer},
   {:human, "Kate", :developer},
   {:cat, "Tihon", :cat}
 ],
 [
   {:topic, :high, "release my_cool_service v1.2.3"},
   {:topic, :medium, "buying food for cat"},
   {:topic, :low, "backlog refinement"}
 ]}
```

Кроме tuple и list есть еще map. И все абстракции построены на базе этих трех типов.

Этот вариант интересен как некий базовый подход, общий для всех функциональных языков. Но, конечно, Эликсир предлагает более удобные средства.

event_with_struct.exs:
```
c("06_01_event.ex", "ebin")

%Lesson_06.Task_06_01_Event.Event{
  agenda: [
    %Lesson_06.Task_06_01_Event.Topic{
      priority: :high,
      title: "release my_cool_service v1.2.3"
    },
    %Lesson_06.Task_06_01_Event.Topic{
      priority: :medium,
      title: "buying food for cat"
    },
    %Lesson_06.Task_06_01_Event.Topic{
      priority: :low,
      title: "backlog refinement"
    }
  ],
  datetime: ~U[2021-03-10 19:40:00.000000Z],
  location: %Lesson_06.Task_06_01_Event.Location{
    address: %Lesson_06.Task_06_01_Event.Address{
      city: "Minsk",
      country: "Belarus",
      house_number: 178,
      street: "Partizanskij pr"
    },
    room: %Lesson_06.Task_06_01_Event.Room{floor: nil, number: 610}
  },
  participants: [
    %Lesson_06.Task_06_01_Event.Participant{
      name: "Helen",
      role: :project_manager,
      species: :human
    },
    %Lesson_06.Task_06_01_Event.Participant{
      name: "Bob",
      role: :developer,
      species: :human
    },
    %Lesson_06.Task_06_01_Event.Participant{
      name: "Kate",
      role: :developer,
      species: :human
    },
    %Lesson_06.Task_06_01_Event.Participant{
      name: "Tihon",
      role: :cat,
      species: :cat
    }
  ],
  title: "Team Meeting"
}
```

Особенности:
- struct должна быть определена внутри модуля
- модуль может содержать только одну struct, и у них общее имял
- struct это абстракция поверх map
- defmodule и struct нельзя определять прямо в iex, нужно это делать в отдельном файле, и потом компилировать 
- дублирование полей: defstruct, enforce_keys, type


## Extract data

```
event.agenda
event.location.address.city
%Event.Event{participants: participants} = event
%Event.Event{participants: [first | _]} = event
%Event.Event{location: %Event.Location{room: room}} = event
room.number
```

## TODO Update Data

first level

deeper levels, manually

deeper levels, with put_in, update_in