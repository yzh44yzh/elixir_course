defmodule Solution5 do
  alias BookShop, as: BS
  alias Solution4, as: S4

  def main() do
    BS.test_data() |> handle
  end

  @spec handle(BS.json) :: {:ok, BS.Order.t} | {:error, term}
  def handle(data) do
    state = %{incoming_data: data}
    FP.pipeline(state, [
          &S4.step1/1,
          &S4.step2/1,
          &S4.step3/1,
          &S4.step4/1,
          &S4.step5/1
        ])
  end

  
end
