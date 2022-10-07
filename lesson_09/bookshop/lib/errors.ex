defmodule BookShop.Error do
  
  defmodule InvalidDataError do

    defexception []
    
    @impl true
    def exception(_), do: %InvalidDataError{}

    @impl true
    def message(_), do: "invalid data"

  end


  defmodule CatNotFoundError do

    defexception [:cat_name]
    
    @impl true
    def exception(cat_name), do: %CatNotFoundError{cat_name: cat_name}

    @impl true
    def message(error), do: "cat '#{error.cat_name}' is not found"
    
  end


  defmodule InvalidAddressError do

    defexception []
    
    @impl true
    def exception(_), do: %InvalidAddressError{}

    @impl true
    def message(_), do: "invalid address"

  end


  defmodule BookNotFoundError do

    defexception [:title]
    
    @impl true
    def exception(title), do: %BookNotFoundError{title: title}


    @impl true
    def message(error), do: "book '#{error.title}' is not found"

  end

end
