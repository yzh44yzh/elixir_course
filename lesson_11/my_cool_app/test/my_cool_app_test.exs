defmodule MyCoolAppTest do
  use ExUnit.Case
  doctest MyCoolApp

  test "greets the world" do
    assert MyCoolApp.hello() == :world
  end
end
