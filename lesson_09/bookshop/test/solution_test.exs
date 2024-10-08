defmodule Bookshop.SolutionTest do
  use ExUnit.Case

  alias Bookshop.Model, as: M
  alias Bookshop.Solution3, as: S

  test "create order" do
    valid_data = TestData.valid_data()

    assert S.handle(valid_data) ==
             {:ok,
              %M.Order{
                client: %M.Cat{id: "Tihon", name: "Tihon"},
                address: %M.Address{
                  state: nil,
                  city: nil,
                  other: "Coolcat str 7/42 Minsk Belarus"
                },
                books: [
                  %M.Book{
                    title: "Distributed systems for fun and profit",
                    author: "Mikito Takada"
                  },
                  %M.Book{title: "Domain Modeling Made Functional", author: "Scott Wlaschin"},
                  %M.Book{title: "Удовольствие от Х", author: "Стивен Строгац"}
                ]
              }}
  end

  test "invalid incoming data" do
    invalid_data = TestData.invalid_data()

    assert S.handle(invalid_data) == {:error, :invalid_incoming_data}
  end

  test "invalid cat" do
    data =
      TestData.valid_data()
      |> Map.put("cat", "Baton")

    assert S.handle(data) == {:error, :cat_not_found}
  end

  test "invalid address" do
    data =
      TestData.valid_data()
      |> Map.put("address", "42")

    assert S.handle(data) == {:error, :invalid_address}
  end

  test "invalid book" do
    invalid_book = TestData.invalid_book()

    data =
      TestData.valid_data()
      |> update_in(["books"], fn books -> [invalid_book | books] end)

    assert S.handle(data) == {:error, :book_not_found}
  end
end
