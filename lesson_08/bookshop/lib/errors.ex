defmodule InvalidDataError do

  defexception [
    :message
  ]
    
  @impl true
  def exception(_) do
    %InvalidDataError{message: "invalid data"}
  end

end


defmodule CatNotFoundError do

  defexception [
    :message,
    :cat_name
  ]
    
  @impl true
  def exception(cat_name) do
    msg = "cat \"#{cat_name}\" is not found"
    %CatNotFoundError{message: msg, cat_name: cat_name}
  end

end


defmodule InvalidAddressError do

  defexception [
    :message
  ]
    
  @impl true
  def exception(_) do
    %InvalidAddressError{message: "invalid address"}
  end

end


defmodule BookNotFoundError do

  defexception [
    :message,
    :title
  ]
    
  @impl true
  def exception(title) do
    msg = "book \"#{title}\" is not found"
    %BookNotFoundError{message: msg, title: title}
  end

end


