ExUnit.start()

defmodule Test do
  use ExUnit.Case
  import TicTacToe

  test "valid_game? positive test" do
    assert valid_game?({{:x, :x, :x}, {:x, :x, :x}, {:x, :x, :x}})
    assert valid_game?({{:o, :o, :o}, {:o, :o, :o}, {:o, :o, :o}})
    assert valid_game?({{:f, :f, :f}, {:f, :f, :f}, {:f, :f, :f}})
    assert valid_game?({{:x, :o, :f}, {:f, :x, :o}, {:o, :o, :x}})
  end

  test "valid_game? negative test" do
    assert not valid_game?({{:x, :o, :some}, {:f, :x, :o}, {:o, :o, :x}})
    assert not valid_game?({{:x, :x, :x}, {:x, :some, :x}, {:x, :x, :x}})
    assert not valid_game?({{:o, :o, :o}, {:o, :o, :o}, {:o, :o, :some}})

    assert not valid_game?({{:x, :o, :f}, {:f, :x, :o}, {:o, :o, :x, :x}})
    assert not valid_game?({{:x, :o, :f}, {:f, :x, :x, :o}, {:o, :o, :x}})
    assert not valid_game?({{:x, :o, :x, :f}, {:f, :x, :o}, {:o, :o, :x}})

    assert not valid_game?({{:x, :o, :f}, {:f, :x, :o}, {:o, :o}})
    assert not valid_game?({{:x, :o, :f}, {:f, :o}, {:o, :o, :x}})
    assert not valid_game?({{:x, :o}, {:f, :x, :o}, {:o, :o, :x}})

    assert not valid_game?({{:x, :o, :f}, {:f, :x, :o}, {:o, :o, :x}, {:x, :x, :x}})
    assert not valid_game?({{:x, :o, :f}, {:f, :x, :o}})
  end

  test "check_who_win test" do
    assert {:win, :x} == check_who_win({{:x, :x, :x}, {:f, :f, :o}, {:f, :f, :o}})
    assert {:win, :o} == check_who_win({{:f, :x, :f}, {:o, :o, :o}, {:x, :f, :f}})
    assert {:win, :x} == check_who_win({{:f, :o, :f}, {:o, :f, :f}, {:x, :x, :x}})

    assert {:win, :o} == check_who_win({{:o, :x, :f}, {:o, :f, :x}, {:o, :f, :f}})
    assert {:win, :x} == check_who_win({{:f, :x, :o}, {:p, :x, :f}, {:f, :x, :f}})
    assert {:win, :o} == check_who_win({{:f, :x, :o}, {:f, :x, :o}, {:f, :f, :o}})

    assert {:win, :x} == check_who_win({{:x, :f, :o}, {:o, :x, :f}, {:f, :f, :x}})
    assert {:win, :o} == check_who_win({{:f, :f, :o}, {:x, :o, :f}, {:o, :x, :f}})

    assert :no_win == check_who_win({{:x, :f, :f}, {:f, :x, :x}, {:f, :f, :o}})
    assert :no_win == check_who_win({{:x, :o, :o}, {:o, :x, :x}, {:x, :o, :o}})
  end

end
