defmodule MyCalendarTest do
  use ExUnit.Case
  doctest MyCalendar

  test "greets the world" do
    assert MyCalendar.hello() == :world
  end
end
