defmodule MyCoolServiceTest do
  use ExUnit.Case
  doctest MyCoolService

  test "greets the world" do
    assert MyCoolService.hello() == :world
  end
end
