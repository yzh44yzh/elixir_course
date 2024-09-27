defmodule BookshopTest do
  use ExUnit.Case

  test "greets the world" do
    assert Bookshop.test_data() == TestData.valid_data()
  end
end
