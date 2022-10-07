defmodule Utils do
  
  @spec rand_success() :: boolean()
  def rand_success() do
    rand = :rand.uniform 10
    rand > 1
  end

  def call_many_times(module, num_calls \\ 20) do
    data = BookShop.test_data()
    
    1..num_calls
    |> Enum.map(fn _ -> apply(module, :handle, [data])  end)
  end

  def report(responses) do
    total = length(responses)
    oks = Enum.filter(responses, fn({type, _}) -> type == :ok end)
    errors = Enum.filter(responses, fn({type, _}) -> type == :error end)
    %{
      total: total,
      total_oks: length(oks),
      total_errors: length(errors),
      ok: hd(oks),
      errors: errors
    }
  end

end
