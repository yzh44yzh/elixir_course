defmodule ChatRoomTest do
  use ExUnit.Case
  doctest ChatRoom

  test "User 1 test" do
    assert ChatRoom.join_room("User 1", "Room 1") == :ok
    assert ChatRoom.join_room("User 1", "Room 2") == :ok
    assert ChatRoom.join_room("User 1", "Room 3") == {:error, :room_reached_limit}
    assert ChatRoom.join_room("User 1", "Room 4") == {:error, :room_not_found}
  end

  test "User 2 test" do
    assert ChatRoom.join_room("User 2", "Room 1") == :ok
    assert ChatRoom.join_room("User 2", "Room 2") == :ok
    assert ChatRoom.join_room("User 2", "Room 3") == {:error, :room_reached_limit}
    assert ChatRoom.join_room("User 2", "Room 4") == {:error, :room_not_found}
  end

  test "User 3 test" do
    assert ChatRoom.join_room("User 3", "Room 1") == :ok
    assert ChatRoom.join_room("User 3", "Room 2") == {:error, :not_allowed}
    assert ChatRoom.join_room("User 3", "Room 3") == {:error, :room_reached_limit}
    assert ChatRoom.join_room("User 3", "Room 4") == {:error, :room_not_found}
  end

  test "User 4 test" do
    assert ChatRoom.join_room("User 4", "Room 1") == {:error, :user_not_found}
    assert ChatRoom.join_room("User 4", "Room 2") == {:error, :user_not_found}
    assert ChatRoom.join_room("User 4", "Room 3") == {:error, :user_not_found}
    assert ChatRoom.join_room("User 4", "Room 4") == {:error, :user_not_found}
  end
end
