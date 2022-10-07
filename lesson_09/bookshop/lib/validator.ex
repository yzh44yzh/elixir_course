defmodule BookShop.Validator do
  alias BookShop.Model, as: M

  @spec validate_incoming_data(M.json) :: {:ok, M.json} | {:error, :invalid_incoming_data}
  def validate_incoming_data(json_data) do
    case Utils.rand_success() do
      true -> {:ok, json_data}
      false -> {:error, :invalid_incoming_data}
    end
  end

  @spec validate_cat(String.t) :: {:ok, M.Cat.t} | {:error, :cat_not_found}
  def validate_cat(cat_name) do
    case Utils.rand_success() do
      true -> {:ok, M.Cat.new(cat_name)}
      false -> {:error, :cat_not_found}
    end
  end

  @spec validate_address(String.t) :: {:ok, M.Address.t} | {:error, :invalid_address}
  def validate_address(address) do
    case Utils.rand_success() do
      true -> {:ok, M.Address.new(address)}
      false -> {:error, :invalid_address}
    end
  end

end
