defmodule Solution5 do
  alias BookShop.Model, as: M
  alias Solution4, as: S4

  @spec handle(M.json) :: {:ok, M.Order.t} | {:error, term}
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
