defmodule Solution6 do

  alias BookShop.Validator, as: V

  @spec main() :: {:ok, BookShop.Order.t} | {:error, term}
  def main() do
    with(
      {:ok, %{
          "cat" => cat0,
          "address" => address0,
          "books" => books0,
       }} <- V.validate_incoming_data(BookShop.test_data),
      
      {:ok, cat} <- V.validate_cat(cat0),
      {:ok, address} <- V.validate_address(address0),
      
      {:ok, books} <- Enum.map(
        books0,
        fn(%{"title" => title, "author" => author})->
          BookShop.Book.get_book(title, author)
        end)
        |> FP.sequence()
    ) do
      BookShop.Order.new(cat, address, books)
    end
  end

end
