defmodule AnalyseFruitsNegativeTest do
  use ExUnit.Case

  alias AnalyseFruitsMP, as: A

  setup_all context do
    Logger.configure(level: :emergency) # don't write crashes to standard output
    context
  end
  

  test "data_4 invalid file" do
    files = [
      "./data/data_1.csv",
      "./data/data_2.csv",
      "./data/data_3.csv",
      "./data/data_4.csv"
    ]
    res = assert A.start(files)
    {:error, {{:badmatch, _}, _}} = res

    res = assert A.start(files, 2)
    {:error, {{:badmatch, _}, _}} = res
  end

  test "data_5 not existing file" do
    files = [
      "./data/data_1.csv",
      "./data/data_2.csv",
      "./data/data_3.csv",
      "./data/data_5.csv"
    ]
    res = assert A.start(files)
    {:error, {%File.Error{reason: :enoent}, _}} = res

    res = assert A.start(files, 2)
    {:error, {%File.Error{reason: :enoent}, _}} = res
  end
  

end
