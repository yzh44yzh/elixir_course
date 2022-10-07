defmodule Solution3 do

  alias BookShop, as: BS
  alias BookShop.ValidatorEx, as: V

  @spec main :: {:ok, BookShop.Order.t} | {:error, term}
  def main() do
    BookShop.test_data |> handle()
  end

  def call_handle_many_times do
    0..20 |> Enum.map(fn _ -> main() end)
  end

  @spec handle(BS.json) :: {:ok, BS.Order.t} | {:error, term}
  def handle(data) do
    try do
      data = V.validate_incoming_data!(data)
      %{
        "cat" => cat,
        "address" => address,
        "books" => books
      } = data
      cat = V.validate_cat!(cat)
      address = V.validate_address!(address)
      books = Enum.map(
        books,
        fn(%{"title" => title, "author" => author}) ->
          BS.Book.get_book!(title, author)
        end)
      order = BS.Order.new(cat, address, books)
      {:ok, order}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

end
