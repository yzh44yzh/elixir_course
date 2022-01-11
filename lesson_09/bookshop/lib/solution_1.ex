defmodule Solution1 do

  alias BookShop, as: BS
  alias BookShop.Validator, as: V
  
  @spec main :: {:ok, BS.Order.t} | {:error, term}
  def main() do
    BS.test_data() |> handle
  end

  @spec handle(BS.json) :: {:ok, BS.Order.t} | {:error, term}
  def handle(data) do
    case V.validate_incoming_data(data) do
      {:ok, data} ->
        %{"cat" => cat_name} = data
        case V.validate_cat(cat_name) do
          {:ok, cat} ->
            %{"address" => address_str} = data
            case V.validate_address(address_str) do
              {:ok, address} ->
                %{"books" => books} = data
                maybe_books = Enum.map(
                  books,
                  fn(%{"author" => author, "title" => title}) ->
                    BS.Book.get_book(author, title)
                  end)
                invalid_books = Enum.filter(
                  maybe_books,
                  fn
                    ({:ok, _}) -> false
                    ({:error, _}) -> true
                  end)
                case invalid_books do
                  [] ->
                    books = Enum.map(maybe_books, fn({:ok, book}) -> book end)
                    order = BS.Order.new(cat, address, books)
                    {:ok, order}
                  [error | _] -> error
                end
              {:error, error} -> {:error, error}
            end
          {:error, error} -> {:error, error}
        end
      {:error, error} -> {:error, error}
    end
  end
  
end

