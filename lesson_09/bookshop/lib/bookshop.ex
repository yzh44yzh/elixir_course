defmodule BookShop do
  alias BookShopError, as: E

  @type json :: map
  @type cat :: {:cat, String.t}
  @type address :: {:address, String.t}

  defmodule Book do
    @type t :: %__MODULE__{
      id: String.t,
      title: String.t,
      author: String.t
    }
    @enforce_keys [:id, :title, :author]
    defstruct [:id, :title, :author]

    @spec get_book(String.t, String.t) :: {:ok, t} | {:error, {:book_not_found, String.t}}
    def get_book(author, title) do
      case Utils.rand_success() do
        true ->
          id = "ISBN 978-5-00057-917-6"
          book = %__MODULE__{id: id, author: author, title: title}
          {:ok, book}
        false -> {:error, {:book_not_found, title}}
      end
    end
    
    @spec get_book!(String.t, String.t) :: t
    def get_book!(author, title) do
      case Utils.rand_success() do
        true ->
          id = "ISBN 978-5-00057-917-6"
          %__MODULE__{id: id, author: author, title: title}
        false -> raise E.BookNotFoundError, title
      end
    end
    
  end

  defmodule Order do
    alias BookShop, as: BS
    
    @type t :: %Order{
      customer: BS.cat,
      shipping_address: BS.address,
      books: [Book.t]
    }
    @enforce_keys [:customer, :shipping_address, :books]
    defstruct [:customer, :shipping_address, :books]

    @spec new(BS.cat, BS.address, [BS.Book.t]) :: t
    def new(cat, address, books) do
      %__MODULE__{
        customer: cat,
        shipping_address: address,
        books: books
      }
    end
    
  end


  @spec test_data() :: json
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


  defmodule Validator do
    alias BookShop, as: BS

    @spec validate_incoming_data(BS.json) :: {:ok, BS.json} | {:error, :invalid_incoming_data}
    def validate_incoming_data(json_data) do
      case Utils.rand_success() do
        true -> {:ok, json_data}
        false -> {:error, :invalid_incoming_data}
      end
    end

    @spec validate_cat(String.t) :: {:ok, BS.cat} | {:error, :cat_not_found}
    def validate_cat(cat_name) do
      case Utils.rand_success() do
        true -> {:ok, {:cat, cat_name}}
        false -> {:error, :cat_not_found}
      end
    end

    @spec validate_address(String.t) :: {:ok, BS.address} | {:error, :invalid_address}
    def validate_address(address) do
      case Utils.rand_success() do
        true -> {:ok, {:address, address}}
        false -> {:error, :invalid_address}
      end
    end

  end


  defmodule ValidatorEx do
    alias BookShop, as: BS
    alias BookShopError, as: E
    
    @spec validate_incoming_data!(BS.json) :: BS.json
    def validate_incoming_data!(json_data) do
      case Utils.rand_success() do
        true -> json_data
        false -> raise E.InvalidDataError
      end
    end

    @spec validate_cat!(String.t) :: BS.cat
    def validate_cat!(cat_name) do
      case Utils.rand_success() do
        true -> {:cat, cat_name}
        false -> raise E.CatNotFoundError, cat_name
      end
    end

    @spec validate_address!(String.t) :: BS.address
    def validate_address!(address) do
      case Utils.rand_success() do
        true -> {:address, address}
        false -> raise E.InvalidAddressError
      end
    end

  end

end
