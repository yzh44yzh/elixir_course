# ChatRoom

В этом задании нужно реализовать валидацию входа пользователя в комнату чата.

Вход выполняется по правилам:

1. Комнаты бывают открытые (`:public`) и закрытые (`:private`). В открытые комнаты могут входить любые пользователи. Закрытые комнаты имеют список пользователей, которым разрешен вход. Если в комнату пытается войти пользователь, которому не разрешен доступ, то должна вернуться ошибка `:not_allowed`.

2. Все комнаты имеют лимит на количество пользователей, которые могут находиться в ней одновременно. Если лимит превышается, то должна вернуться ошибка `:room_reached_limit`.

Нужно реализовать функцию:

```
@spec join_room(user_name :: String.t, room_name :: String.t) :: :ok | {:error, atom}
```

В успешном случае функция возвращает `:ok`. Учитывая правила входа и то, что имя пользователя или имя комнаты могут оказаться невалидными, то возможны 4 варианта ошибок:
- :user_not_found
- :room_not_found
- :not_allowed
- :room_reached_limit

Объекты **Room** и **User** описаны в chat_room_model.ex:
```
defmodule ChatRoomModel do

  defmodule User do
  ...
  
  defmodule Room do
  ...
```


В chat_room.ex описаны валидные пользователи и комнаты:
```
  @users [
    %M.User{name: "User 1"},
    %M.User{name: "User 2"},
    %M.User{name: "User 3"}
  ]
  
  @rooms [
    %M.Room{name: "Room 1", type: :public},
    %M.Room{name: "Room 2", type: :private, members: ["User 1", "User 2"]},
    %M.Room{name: "Room 3", type: :public, limit: 10},
  ]
```

И реализованы функции валидации:

```
@spec get_user(String.t) :: {:ok, M.User.t} | {:error, :not_found}
@spec get_room(String.t) :: {:ok, M.Room.t} | {:error, :not_found}
@spec public?(M.Room.t) :: boolean
@spec member?(M.User.t, M.Room.t) :: boolean
@spec reached_limit?(M.Room.t) :: boolean
```

Ваша задача скомпоновать функции валидации внутри `join_room/2`. Используйте любой вариант компоновки, какой вы считаете подходящим.

Смотрите в тестах test/chat_room_test.exs, какие ожидаются ответы из join_room.
