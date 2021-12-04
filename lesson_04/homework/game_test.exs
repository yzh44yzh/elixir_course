ExUnit.start()

defmodule Test do
  use ExUnit.Case
  import Game

  test "join_game test" do
    assert :ok == join_game({:user, "Bob", 17, :admin})
    assert :ok == join_game({:user, "Bob", 27, :admin})
    assert :ok == join_game({:user, "Bob", 17, :moderator})
    assert :ok == join_game({:user, "Bob", 27, :moderator})
    assert :error == join_game({:user, "Bob", 17, :member})
    assert :ok == join_game({:user, "Bob", 27, :member})
  end

  test "move_allowed? test" do
    assert move_allowed?(:white, {:pawn, :white})
    assert not move_allowed?(:black, {:pawn, :white})
    assert move_allowed?(:white, {:rock, :white})
    assert not move_allowed?(:black, {:rock, :white})
    assert not move_allowed?(:white, {:queen, :white})
    assert not move_allowed?(:black, {:queen, :white})
  end

  test "single_win? test" do
    assert single_win?(true, false)
    assert single_win?(false, true)
    assert not single_win?(true, true)
    assert not single_win?(false, false)
  end

  test "double_win? test" do
    assert :ab == double_win?(true, true, false)
    assert :bc == double_win?(false, true, true)
    assert :ac == double_win?(true, false, true)
    assert not double_win?(true, true, true)
    assert not double_win?(false, false, false)
    assert not double_win?(true, false, false)
    assert not double_win?(false, true, false)
    assert not double_win?(false, false, true)
  end

end
