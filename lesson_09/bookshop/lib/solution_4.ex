defmodule Bookshop.Solution4 do
  alias Bookshop.Model, as: M
  alias Bookshop.Controller, as: C

  def handle(data) do
    f =
      FP.bind(&C.validate_incoming_data/1, &step_validate_cat/1)
      |> FP.bind(&step_validate_address/1)
      |> FP.bind(&step_validate_books/1)
      |> FP.bind(&step_create_order/1)

    f.(data)
  end

  def step_validate_cat(data) do
    case C.validate_cat(data["cat"]) do
      {:ok, cat} ->
        state = %{data: data, cat: cat}
        {:ok, state}

      error ->
        error
    end
  end

  def step_validate_address(state) do
    case C.validate_address(state.data["address"]) do
      {:ok, address} ->
        state = Map.put(state, :address, address)
        {:ok, state}

      error ->
        error
    end
  end

  def step_validate_books(state) do
    state.data["books"]
    |> Enum.map(&C.validate_book/1)
    |> FP.sequence()
    |> case do
      {:ok, books} ->
        state = Map.put(state, :books, books)
        {:ok, state}

      error ->
        error
    end
  end

  def step_create_order(state) do
    {:ok, M.Order.create(state.cat, state.address, state.books)}
  end
end
