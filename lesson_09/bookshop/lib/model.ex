defmodule BookShop.Model do

  @type json :: map

  defmodule Cat do
    @type t :: {:cat, name: String.t}

    def new(name) do
      {:cat, name}
    end
  end

  defmodule Address do
    @type t :: {:address, String.t}

    def new(address) do
      {:address, address}
    end
  end

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
        false -> raise BookShop.Error.BookNotFoundError, title
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
end
