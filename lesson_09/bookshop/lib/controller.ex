defmodule Bookshop.Controller do
  alias Bookshop.Model, as: M
  alias Bookshop.Errors, as: E

  # def handle(request) do
  # end

  @existing_cats ["Tihon", "Marfa", "Plushka"]
  @existing_authors ["Scott Wlaschin", "Стивен Строгац", "Mikito Takada"]

  @spec validate_incoming_data(map()) :: {:ok, map()} | {:error, :invalid_incoming_data}
  def validate_incoming_data(%{"cat" => _, "address" => _, "books" => _} = data) do
    {:ok, data}
  end

  def validate_incoming_data(_) do
    {:error, :invalid_incoming_data}
  end

  @spec validate_cat(name :: String.t()) :: {:ok, Cat.t()} | {:error, :cat_not_found}
  def validate_cat(name) do
    if name in @existing_cats do
      {:ok, %M.Cat{id: name, name: name}}
    else
      {:error, :cat_not_found}
    end
  end

  @spec validate_address(String.t()) :: {:ok, Address.t()} | {:error, :invalid_address}
  def validate_address(data) do
    if String.length(data) > 5 do
      {:ok, %M.Address{other: data}}
    else
      {:error, :invalid_address}
    end
  end

  @spec validate_book(map()) :: {:ok, Book.t()} | {:error, :book_not_found}
  def validate_book(%{"author" => author} = data) do
    if author in @existing_authors do
      {:ok, %M.Book{title: data["title"], author: data["author"]}}
    else
      {:error, :book_not_found}
    end
  end

  @spec validate_incoming_data!(map()) :: map()
  def validate_incoming_data!(%{"cat" => _, "address" => _, "books" => _} = data) do
    data
  end

  def validate_incoming_data!(_) do
    raise E.InvalidIncomingData
  end

  @spec validate_cat!(name :: String.t()) :: Cat.t()
  def validate_cat!(name) do
    if name in @existing_cats do
      %M.Cat{id: name, name: name}
    else
      raise E.CatNotFound, name
    end
  end

  @spec validate_address!(String.t()) :: Address.t()
  def validate_address!(data) do
    if String.length(data) > 5 do
      %M.Address{other: data}
    else
      raise E.InvalidAddress, data
    end
  end

  @spec validate_book!(map()) :: Book.t()
  def validate_book!(%{"author" => author} = data) do
    if author in @existing_authors do
      %M.Book{title: data["title"], author: data["author"]}
    else
      raise E.BookNotFound, {data["title"], data["author"]}
    end
  end
end
