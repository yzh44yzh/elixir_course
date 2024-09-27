defmodule Solution6 do
  alias BookShop.Model, as: M
  alias BookShop.Validator, as: V

  @spec handle(M.json()) :: {:ok, M.Order.t()} | {:error, term}
  def handle(data) do
    with(
      {:ok, data} <- V.validate_incoming_data(data),
      %{"cat" => cat_name, "address" => address_str, "books" => books} = data,
      {:ok, cat} <- V.validate_cat(cat_name),
      {:ok, address} <- V.validate_address(address_str),
      maybe_books =
        Enum.map(
          books,
          fn %{"author" => author, "title" => title} ->
            M.Book.get_book(author, title)
          end
        ),
      {:ok, books} <- FP.sequence(maybe_books)
    ) do
      order = M.Order.new(cat, address, books)
      {:ok, order}
    end
  end
end
