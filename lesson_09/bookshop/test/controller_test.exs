defmodule Bookshop.ControllerTest do
  use ExUnit.Case

  alias Bookshop.Controller, as: C
  alias Bookshop.Model, as: M

  test "validate incoming data" do
    valid_data = TestData.valid_data()
    assert C.validate_incoming_data(valid_data) == {:ok, valid_data}

    invalid_data = TestData.invalid_data()
    assert C.validate_incoming_data(invalid_data) == {:error, :invalid_incoming_data}
  end

  test "validate cat" do
    assert C.validate_cat("Tihon") == {:ok, %M.Cat{id: "Tihon", name: "Tihon"}}
    assert C.validate_cat("Baton") == {:error, :cat_not_found}
  end

  test "validate address" do
    assert C.validate_address("Minsk Belarus") == {
             :ok,
             %M.Address{
               state: nil,
               city: nil,
               other: "Minsk Belarus"
             }
           }

    assert C.validate_address("42") == {:error, :invalid_address}
  end

  test "validate book" do
    valid_book = TestData.valid_book()

    assert C.validate_book(valid_book) == {
             :ok,
             %M.Book{
               title: valid_book["title"],
               author: valid_book["author"]
             }
           }

    invalid_book = TestData.invalid_book()
    assert C.validate_book(invalid_book) == {:error, :book_not_found}
  end
end
