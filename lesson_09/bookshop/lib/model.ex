defmodule Bookshop.Model do
  defmodule Cat do
    defstruct [:id, :name]
  end

  defmodule Address do
    defstruct [:state, :city, :other]
  end

  defmodule Book do
    defstruct [:title, :author]
  end

  defmodule Order do
    defstruct [:client, :address, :books]

    def create(client, address, books) do
      %__MODULE__{
        client: client,
        address: address,
        books: books
      }
    end
  end
end
