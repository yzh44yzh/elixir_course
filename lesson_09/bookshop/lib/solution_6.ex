defmodule Bookshop.Solution6 do
  alias Bookshop.Controller, as: C
  alias Bookshop.Model, as: M

  def handle(data) do
    with {:ok, data} <- C.validate_incoming_data(data),
         %{"cat" => cat_name, "address" => address_str, "books" => book_data} = data,                {:ok, cat} <- C.validate_cat(cat_name),
         {:ok, address} <- C.validate_address(address_str),
         books = Enum.map(book_data, fn data -> C.validate_book(data) end),
         {:ok, books} <- FP.sequence(books) do
      order = M.Order.create(cat, address, books)
      {:ok, order}
    end
  end

end
