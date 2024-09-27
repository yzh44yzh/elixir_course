defmodule Bookshop.Solution1 do
  alias Bookshop.Model, as: M
  alias Bookshop.Controller, as: C

  @spec handle(map()) :: {:ok, M.Order.t()} | {:error, any()}
  def handle(data) do
    case C.validate_incoming_data(data) do
      {:ok, data} ->
        case C.validate_cat(data["cat"]) do
          {:ok, cat} ->
            case C.validate_address(data["address"]) do
              {:ok, address} ->
                data["books"]
                |> Enum.map(&C.validate_book/1)
                |> Enum.reduce({[], nil}, fn
                  {:ok, book}, {books, nil} -> {[book | books], nil}
                  {:error, error}, {books, nil} -> {books, {:error, error}}
                  _maybe_book, acc -> acc
                end)
                |> case do
                  {books, nil} -> {:ok, M.Order.create(cat, address, books)}
                  {_, error} -> error
                end

              {:error, error} ->
                {:error, error}
            end

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end
end
