defmodule BookShop.ValidatorExc do
  alias BookShop.Model, as: M
  alias BookShop.Error, as: E

  @spec validate_incoming_data!(M.json()) :: M.json()
  def validate_incoming_data!(json_data) do
    case Utils.rand_success() do
      true -> json_data
      false -> raise E.InvalidDataError
    end
  end

  @spec validate_cat!(String.t()) :: M.Cat.t()
  def validate_cat!(cat_name) do
    case Utils.rand_success() do
      true -> M.Cat.new(cat_name)
      false -> raise E.CatNotFoundError, cat_name
    end
  end

  @spec validate_address!(String.t()) :: M.Address.t()
  def validate_address!(address) do
    case Utils.rand_success() do
      true -> M.Address.new(address)
      false -> raise E.InvalidAddressError
    end
  end
end
