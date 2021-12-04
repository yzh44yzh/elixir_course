defmodule BookShop do

  # Types

  @type cat :: {:cat, binary()}
  @type address :: {:address, binary()}

  defmodule Book do
    @type t :: %__MODULE__{
      id: binary(),
      title: binary(),
      author: binary()
    }
    @enforce_keys [:id, :title, :author]
    defstruct [
      :id,
      :title,
      :author
    ]
  end

  defmodule Order do
    alias BookShop, as: BS
    @type t :: %Order{
      customer: BS.cat,
      shipping_address: BS.address,
      books: [Book.t]
    }
    @enforce_keys [:customer, :shipping_address, :books]
    defstruct [
      :customer,
      :shipping_address,
      :books
    ]
  end


  # Test Data

  @spec test_data() :: map()
  def test_data() do
    %{
      "cat" => "Tihon",
      "address" => "Coolcat str 7/42 Minsk Belarus",
      "books" => [
        %{
          "title" => "Domain Modeling Made Functional",
          "author" => "Scott Wlaschin"
        },
        %{
          "title" => "Удовольствие от Х",
          "author" => "Стивен Строгац"
        },
        %{
          "title" => "Distributed systems for fun and profit",
          "author" => "Mikito Takada"
        }
      ]
    }
  end


  @spec create_order(cat, address, [Book.t]) :: Order.t
  def create_order(cat, address, books) do
    %Order{customer: cat, shipping_address: address, books: books}
  end

  
  @spec get_book(binary(), binary()) :: {:ok, Book.t} | {:error, {:book_not_found, binary()}}
  def get_book(title, author) do
    case Utils.rand_success() do
      true ->
        {:ok,
         %Book {
           id: "ISBN 978-5-00057-917-6",
           title: title,
           author: author
         }
        }
      false -> {:error, {:book_not_found, title}}
    end
  end

  
  @spec get_book!(binary(), binary()) :: Book.t
  def get_book!(title, author) do
    case Utils.rand_success() do
      true ->
        %Book {
          id: "ISBN 978-5-00057-917-6",
          title: title,
          author: author
}
      false -> raise BookNotFoundError, title
    end
  end


  defmodule Validator do

    @spec validate_incoming_data(map()) :: {:ok, map()} | {:error, :invalid_incoming_data}
    def validate_incoming_data(json_data) do
      case Utils.rand_success() do
        true -> {:ok, json_data}
        false -> {:error, :invalid_incoming_data}
      end
    end


    @spec validate_cat(binary()) :: {:ok, BookShop.cat()} | {:error, :cat_not_found}
    def validate_cat(cat_name) do
      case Utils.rand_success() do
        true -> {:ok, {:cat, cat_name}}
        false -> {:error, :cat_not_found}
      end
    end


    @spec validate_address(binary()) :: {:ok, BookShop.address()} | {:error, :invalid_address}
    def validate_address(address) do
      case Utils.rand_success() do
        true -> {:ok, {:address, address}}
        false -> {:error, :invalid_address}
      end
    end

  end


  defmodule ValidatorEx do

    @spec validate_incoming_data!(map()) :: map()
    def validate_incoming_data!(json_data) do
      case Utils.rand_success() do
        true -> json_data
        false -> raise InvalidDataError
      end
    end


    @spec validate_cat!(binary()) :: BookShop.cat()
    def validate_cat!(cat_name) do
      case Utils.rand_success() do
        true -> {:cat, cat_name}
        false -> raise CatNotFoundError, cat_name
      end
    end


    @spec validate_address!(binary()) :: BookShop.address()
    def validate_address!(address) do
      case Utils.rand_success() do
        true -> {:address, address}
        false -> raise InvalidAddressError
      end
    end

  end

end
