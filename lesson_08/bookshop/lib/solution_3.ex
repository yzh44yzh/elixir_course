defmodule Solution3 do

    alias BookShop.ValidatorEx, as: V

    @spec main :: {:ok, BookShop.Order.t} | {:error, term}
    def main do
        BookShop.test_data |> handle_create_order
    end

    @spec handle_create_order(map) :: {:ok, BookShop.Order.t} | {:error, term}
    def handle_create_order(data0)do
        try do
            data = V.validate_incoming_data!(data0)
            %{
                "cat" => cat0,
                "address" => address0,
                "books" => books0
            } = data
            cat = V.validate_cat!(cat0)
            address = V.validate_address!(address0)
            books = Enum.map(books0,
              fn %{"title" => title, "author" => author } ->
                BookShop.get_book!(title, author)
              end)
            order = BookShop.create_order(cat, address, books)
            {:ok, order}
        rescue
            error -> {:error, error.message}
        end
    end

end
