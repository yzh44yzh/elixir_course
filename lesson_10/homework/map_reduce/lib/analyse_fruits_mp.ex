defmodule AnalyseFruitsMP do
  @moduledoc """
  MapReduce solution
  """

  @type mapper_id :: integer
  @type mapper :: {:mapper, mapper_id, String.t}
  @type reducer_id :: {integer, integer}
  @type children :: [mapper] | [reducer]
  @type reducer :: {:reducer, reducer_id, children}
  @type result :: %{String.t => integer}

  
  @spec test(integer) :: {:ok, result} | {:error, term}
  def test(processes_per_level \\ 2) do
    files = [
      "./data/data_1.csv",
      "./data/data_2.csv",
      "./data/data_3.csv",
      # "./data/data_5.csv"
    ]
    start(files, processes_per_level)
  end

  @spec start([String.t], integer) :: {:ok, result} | {:error, term}
  def start(files, processes_per_level \\ 4) do
    # TODO add your implementation
    {:ok, %{}}
  end

  @spec build_processes_tree([String.t], integer) :: reducer
  def build_processes_tree(files, processes_per_level) do
    # TODO add your implementation
  end


  
  defmodule Coordinator do
    # TODO add your implementation
  end

  
  defmodule Mapper do
    # TODO add your implementation
  end

  
  defmodule Reducer do
    # TODO add your implementation
  end

  
end
