defmodule Bookshop.Solution2 do
  alias Bookshop.Model, as: M
  alias Bookshop.Controller, as: C

  @spec handle(map()) :: {:ok, M.Order.t()} | {:error, any()}
  def handle(data) do
    case C.validate_incoming_data(data) do
      {:ok, data} ->
        handle_cat(data, %{})

      {:error, error} ->
        {:error, error}
    end
  end

  def handle_cat(data, state) do
    case C.validate_cat(data["cat"]) do
      {:ok, cat} ->
        state = Map.put(state, :cat, cat)
        handle_address(data, state)

      {:error, error} ->
        {:error, error}
    end
  end

  def handle_address(data, state) do
    case C.validate_address(data["address"]) do
      {:ok, address} ->
        state = Map.put(state, :address, address)
        handle_books(data, state)

      {:error, error} ->
        {:error, error}
    end
  end

  def handle_books(%{"books" => books}, state) do
    books
    |> Enum.map(&C.validate_book/1)
    |> Enum.reduce({[], nil}, fn
      {:ok, book}, {books, nil} -> {[book | books], nil}
      {:error, error}, {books, nil} -> {books, {:error, error}}
      _maybe_book, acc -> acc
    end)
    |> case do
      {books, nil} -> {:ok, M.Order.create(state.cat, state.address, books)}
      {_, error} -> error
    end
  end
end
