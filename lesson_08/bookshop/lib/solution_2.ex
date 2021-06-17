defmodule Solution2 do

  alias BookShop.Validator, as: V

  @spec main :: {:ok, BookShop.Order.t} | {:error, term}
  def main do
    data = BookShop.test_data
    handle_create_order data, %{}
  end


  @spec handle_create_order(map, map) :: {:ok, BookShop.Order.t} | {:error, term}
  def handle_create_order data0, state do
    case V.validate_incoming_data data0 do
      {:error, reason} -> {:error, reason}
      {:ok, data} -> validate_cat data, state
    end
  end


  @spec validate_cat(map, map) :: {:ok, BookShop.Order.t} | {:error, term}
  def validate_cat %{"cat" => cat0} = data, state0 do
    case V.validate_cat cat0 do
      {:error, reason} -> {:error, reason}
      {:ok, cat} ->
        state = Map.put state0, :cat, cat
        validate_address data, state
    end
  end


  @spec validate_address(map, map) :: {:ok, BookShop.Order.t} | {:error, term}
  def validate_address %{"address" => address0} = data, state0 do
    case V.validate_address address0 do
      {:error, reason} -> {:error, reason}
      {:ok, address} ->
        state = Map.put state0, :address, address
        validate_books data, state
    end
  end


  @spec validate_books(map, map) :: {:ok, BookShop.Order.t} | {:error, term}
  def validate_books %{"books" => books0}, state0 do
    books = Enum.map books0,
    fn %{"title" => title, "author" => author} ->
      BookShop.get_book title, author
    end
    invalid_books = Enum.filter books, fn {res, _} -> res == :error end
    case invalid_books do
      [{:error, reason} | _] -> {:error, reason}
      [] ->
        state = Map.put state0, :books, books
        create_order state
    end
  end


  @spec create_order(map) :: {:ok, BookShop.Order.t}
  def create_order %{:cat => cat, :address => address, :books => books0} do
    books = Enum.map books0, fn {:ok, book} -> book end
    order = BookShop.create_order cat, address, books
    {:ok, order}
  end

end
