defmodule Bookshop.Errors do

  defmodule InvalidIncomingData do
    defexception []

    @impl true
    def exception(_), do: %InvalidIncomingData{}

    @impl true
    def message(_exc), do: "InvalidIncomingData"
  end

  defmodule CatNotFound do
    defexception [:name]

    @impl true
    def exception(name), do: %CatNotFound{name: name}

    @impl true
    def message(exc), do: "CatNotFound: #{exc.name}"
  end

  defmodule InvalidAddress do
    defexception [:data]

    @impl true
    def exception(data), do: %InvalidAddress{data: data}

    @impl true
    def message(exc), do: "InvalidAddress: #{exc.data}"
  end

  defmodule BookNotFound do
    defexception [:title, :author]

    @impl true
    def exception({title, author}), do: %BookNotFound{title: title, author: author}

    @impl true
    def message(exc), do: "BookNotFound: #{exc.title} #{exc.author}"
  end

  def description(%InvalidIncomingData{}), do: :invalid_incoming_data
  def description(%CatNotFound{}), do: :cat_not_found
  def description(%InvalidAddress{}), do: :invalid_address
  def description(%BookNotFound{}), do: :book_not_found
end
