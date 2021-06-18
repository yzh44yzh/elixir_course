defmodule Solution5 do

  alias BookShop.Validator, as: V
  alias Solution4, as: S4

  @spec main() :: {:ok, BookShop.Order.t} | {:error, term}
  def main() do
    FP.pipeline(BookShop.test_data,               
      [
        &V.validate_incoming_data/1,
        &S4.validate_cat/1,
        &S4.validate_address/1,
        &S4.validate_books/1,
        &S4.create_order/1
      ])
  end

end
