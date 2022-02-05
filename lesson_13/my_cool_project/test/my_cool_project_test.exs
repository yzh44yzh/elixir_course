defmodule MyCoolProjectTest do
  use ExUnit.Case
  doctest MyCoolProject

  test "greets the world" do
    assert MyCoolProject.hello() == :world
  end
end
