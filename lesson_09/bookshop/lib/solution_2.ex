defmodule Solution2 do

  alias BookShop, as: BS
  alias BookShop.Validator, as: V

  @spec main :: {:ok, BS.Order.t} | {:error, term}
  def main() do
    BS.test_data |> handle()
  end

  @spec handle(BS.json) :: {:ok, BS.Order.t} | {:error, term}
  def handle(data) do
    state = %{incoming_data: data}
    step1(state)
  end

  def step1(%{incoming_data: data} = state) do
    case V.validate_incoming_data(data) do
      {:ok, valid_data} ->
        state = %{state | incoming_data: valid_data}
        step2(state)
      {:error, error} -> {:error, error}
    end
  end

  def step2(%{incoming_data: data} = state) do
    %{"cat" => cat_name} = data
    case V.validate_cat(cat_name) do
      {:ok, cat} ->
        state = Map.put(state, :cat, cat)
        step3(state)
      {:error, error} -> {:error, error}
    end
  end

  def step3(%{incoming_data: data} = state) do
    %{"address" => address_str} = data
    case V.validate_address(address_str) do
      {:ok, address} ->
        state = Map.put(state, :address, address)
        step4(state)
      {:error, error} -> {:error, error}
    end
  end

  def step4(%{incoming_data: data} = state) do
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
        state = Map.put(state, :books, books)
        step5(state)
      [error | _] -> error
    end
  end

  def step5(%{cat: cat, address: address, books: books}) do
    order = BS.Order.new(cat, address, books)
    {:ok, order}
  end

end
