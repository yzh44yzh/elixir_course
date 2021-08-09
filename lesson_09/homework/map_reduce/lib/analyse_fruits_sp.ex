defmodule AnalyseFruitsSP do
  @moduledoc """
  Single process solution
  """
  
  @type result :: %{String.t => integer}

  @spec start() :: result
  def start() do
    start([
      "./data/data_1.csv",
      "./data/data_2.csv",
      "./data/data_3.csv"
    ])
  end

  @spec start([String.t]) :: result
  def start(files) do
    # TODO add your implementation
    %{}
  end

  
end
