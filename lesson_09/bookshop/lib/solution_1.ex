defmodule Solution1 do

  alias BookShop.Validator, as: V
  
  @spec main :: {:ok, BookShop.Order.t} | {:error, term}
  def main() do
    BookShop.test_data |> handle_create_order
  end


  @spec handle_create_order(map) :: {:ok, BookShop.Order.t} | {:error, term}
  def handle_create_order(data) do
    case V.validate_incoming_data(data) do
      {:error, reason} -> {:error, reason}
      {:ok, %{
          "cat" => cat0,
          "address" => address0,
          "books" => books0
       }} ->
          case V.validate_cat(cat0) do
            {:error, reason} -> {:error, reason}
            {:ok, cat} ->
              case V.validate_address(address0) do
                {:error, reason} -> {:error, reason}
                {:ok, address} ->
                  books1 = Enum.map(books0, 
                  fn %{"title" => title, "author" => author} ->
                    BookShop.get_book(title, author)
                  end)
                  invalid_books = Enum.filter(books1, fn ({res, _}) -> res == :error end)
                  case invalid_books do
                    [{:error, reason} | _] -> {:error, reason}
                    [] ->
                      books2 = Enum.map(books1, fn ({:ok, book}) -> book end)
                      BookShop.create_order(cat, address, books2)
                  end
              end
          end
    end
  end

end

