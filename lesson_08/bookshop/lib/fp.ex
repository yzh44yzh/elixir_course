defmodule FP do

  alias BookShop.Validator, as: V

  def main() do
    f = bind(&V.validate_incoming_data/1, &V.validate_cat/1)
    f.(BookShop.test_data)
  end

  
  def bind(f1, f2) do
    fn(args) ->
      case f1.(args) do
        {:ok, res} -> f2.(res)
        {:error, reason} -> {:error, reason}
      end
    end
  end
  
end
