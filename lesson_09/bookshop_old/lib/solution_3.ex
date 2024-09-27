defmodule Solution3 do

  alias BookShop.Model, as: M
  alias BookShop.ValidatorExc, as: V

  @spec handle(M.json) :: {:ok, M.Order.t} | {:error, term}
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
          M.Book.get_book!(title, author)
        end)
      order = M.Order.new(cat, address, books)
      {:ok, order}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

end
