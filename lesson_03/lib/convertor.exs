defmodule Convertor do

  def database() do
    [
      {"C1", "C2", 1.5, 1.6},
      {"C1", "C3", 2.5, 2.6},
      {"C2", "C3", 1.05, 1.07}
    ]
  end

  def init(rows) do
    init(rows, %{})
  end

  def init([], acc), do: acc

  def init([row | rows], acc) do
    {c1, c2, k1, k2} = row

    new_acc =
      acc
      |> Map.put({c1, c2}, k1)
      |> Map.put({c2, c1}, k2)

    init(rows, new_acc)
  end

  def convert(currency_info, amount, from_curr, to_curr) do
    case Map.fetch(currency_info, {from_curr, to_curr}) do
      {:ok, coef} -> {:ok, amount * coef}
      :error -> :not_found
    end
  end

end

ExUnit.start()

defmodule ConvertorTest do
  use ExUnit.Case

  test "init" do
    assert Convertor.init([]) == %{}
    assert Convertor.init([{"usd", "eur", 1.1, 0.9}]) == %{
             {"usd", "eur"} => 1.1,
             {"eur", "usd"} => 0.9
           }
    assert Convertor.init(
             [
               {"usd", "eur", 1.1, 0.9},
               {"gbp", "eur", 1.2, 0.8}
             ]
           ) == %{
             {"usd", "eur"} => 1.1,
             {"eur", "usd"} => 0.9,
             {"gbp", "eur"} => 1.2,
             {"eur", "gbp"} => 0.8
           }
  end

  test "convert" do
    state = %{
      {"usd", "eur"} => 1.1,
      {"eur", "usd"} => 0.9,
      {"gbp", "eur"} => 1.2,
      {"eur", "gbp"} => 0.8
    }

    assert Convertor.convert(state, 1000, "usd", "eur") == {:ok, 1100}
    assert Convertor.convert(state, 10_000, "eur", "gbp") == {:ok, 8_000}
  end
end
