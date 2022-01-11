defmodule Solution4 do

  alias BookShop.Validator, as: V

  @spec main() :: {:ok, BookShop.Order.t} | {:error, term}
  def main() do
    f = FP.bind(FP.bind(FP.bind(FP.bind(
                  &V.validate_incoming_data/1, &validate_cat/1), &validate_address/1), &validate_books/1), &create_order/1)
    f.(BookShop.test_data)
  end
  
  @spec validate_cat(map) :: {:ok, map} | {:error, term}
  def validate_cat(%{"cat" => cat0} = state) do
    case V.validate_cat(cat0) do
      {:ok, cat} -> {:ok, Map.put(state, :cat, cat)}
      error -> error
    end
  end


  @spec validate_address(map) :: {:ok, map} | {:error, term}
  def validate_address(%{"address" => address0} = state) do
    case V.validate_address(address0) do
      {:ok, address} -> {:ok, Map.put(state, :address, address)}
      error -> error
    end
  end


  @spec validate_books(map) :: {:ok, map} | {:error, term}
  def validate_books(%{"books" => books0} = state) do
    books1 = Enum.map(books0,
      fn %{"title" => title, "author" => author} ->
        BookShop.Book.get_book(title, author)
      end)
    case FP.sequence(books1) do
      {:ok, books2} -> {:ok, Map.put(state, :books, books2)}
      error -> error
    end
  end


  @spec create_order(map) :: {:ok, BookShop.Order.t}
  def create_order(%{:cat => cat, :address => address, :books => books}) do
    order = BookShop.Order.new(cat, address, books)
    {:ok, order}
  end

end
