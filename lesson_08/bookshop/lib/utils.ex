defmodule Utils do
  
  @spec rand_success() :: boolean()
  def rand_success() do
    rand = :rand.uniform 10
    rand > 1
  end

end
