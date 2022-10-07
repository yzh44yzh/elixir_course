defmodule FP do

  alias BookShop.Validator, as: V

  @type success_val :: any
  @type error_val :: any
  @type result(s, e) :: {:ok, s} | {:error, e}

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

  def bind(funs) do
    fn(arg) -> pipeline(arg, funs) end
  end

  def pipeline(state, fun_list) do
    Enum.reduce(
      fun_list,
      {:ok, state},
      fn
        (f, {:ok, state}) -> f.(state)
        (_, {:error, reason}) -> {:error, reason}
      end)
  end

  @spec sequence([result(success_val, error_val)]) :: result([success_val], error_val)
  def sequence(list) do
    Enum.reduce(
      list,
      {:ok, []},
      fn
        (_, {:error, _} = acc) -> acc
        ({:ok, v}, {:ok, acc}) -> {:ok, [v | acc]}
        ({:error, _} = e, _) -> e
      end
    )
  end

  def not_curried_fun(a, b, c) do
    a + b + c
  end

  def curried_fun(a) do
    fn(b) ->
      fn(c) -> a + b + c end
    end
  end
  
end
