defmodule TestData do
  def valid_data do
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

  def invalid_data do
    %{
      "dog" => "Tihon",
      "address" => "Coolcat str 7/42 Minsk Belarus"
    }
  end

  def valid_book do
    %{
      "title" => "Удовольствие от Х",
      "author" => "Стивен Строгац"
    }
  end

  def invalid_book do
    %{
      "title" => "Functional Web Development with Elixir, OTP, and Phoenix",
      "author" => "Lance Halvorsen"
    }
  end
end
