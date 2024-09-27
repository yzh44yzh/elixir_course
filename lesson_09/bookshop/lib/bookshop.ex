defmodule Bookshop do
  def test_data do
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
end
