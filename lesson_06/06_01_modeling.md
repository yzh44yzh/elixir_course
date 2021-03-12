# Моделирование предметной области

Давайте смоделируем такой достаточно сложный объект, как "встреча" (митинг, совещание).

Митинг состоит из:
- место
- время
- участники
- агенда

TODO нарисовать схему

В принципе, для моделирования сущностей любой сложности достаточно кортежей и списком. И в нашем случае это может выглядеть так:
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

Этот вариант интересен как некий базовый подход, общий для всех функциональных языков. Но, конечно, Эликсир предлагает более удобные средства.

```
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
- модуль и struct один к одному
- defmodule и struct нельзя определять прямо в iex, нужно это делать в отдельном файле, и потом компилировать 
- дублирование полей: defstruct, enforce_keys, type


