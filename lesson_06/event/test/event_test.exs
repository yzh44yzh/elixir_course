defmodule EventTest do
  use ExUnit.Case
  doctest Event

  test "greets the world" do
    assert Event.hello() == :world
  end
end
