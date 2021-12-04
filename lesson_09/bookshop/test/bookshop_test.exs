defmodule BookshopTest do
  use ExUnit.Case
  doctest Bookshop

  test "greets the world" do
    assert Bookshop.hello() == :world
  end
end
