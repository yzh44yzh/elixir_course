defmodule Solution6 do
  alias BookShop, as: BS
  alias BookShop.Validator, as: V

  def main() do
    BS.test_data() |> handle
  end

  def call_handle_many_times do
    0..20 |> Enum.map(fn _ -> main() end)
  end

  @spec handle(BS.json) :: {:ok, BS.Order.t} | {:error, term}
  def handle(data) do
    with(
      {:ok, data} <- V.validate_incoming_data(data),
      %{"cat" => cat_name, "address" => address_str, "books" => books} = data,
      {:ok, cat} <- V.validate_cat(cat_name),
      {:ok, address} <- V.validate_address(address_str),
      maybe_books = Enum.map(
        books,
        fn(%{"author" => author, "title" => title}) ->
          BS.Book.get_book(author, title)
        end),
      {:ok, books} <- FP.sequence(maybe_books)
    ) do
      order = BS.Order.new(cat, address, books)
      {:ok, order}
    end
  end
  
end
