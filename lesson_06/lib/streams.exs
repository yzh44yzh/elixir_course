defmodule StreamExamples do

  def enum_example() do
    list = [1,2,3,4,5]
    Enum.map(list, fn(i) -> i * 2 end)
    |> Enum.zip([:a, :b, :c, :d, :e])
    |> Enum.filter(fn({a, _b}) -> a > 2 end)
  end

  def stream_example() do
    list = [1,2,3,4,5]
    Stream.map(list, fn(i) -> i * 2 end)
    |> Stream.zip([:a, :b, :c, :d, :e])
    |> Stream.filter(fn({a, _b}) -> a > 2 end)
  end


  @data_file "./data.txt"

  def find_longest(file \\ @data_file) do
    File.read!(file)
    |> String.split("\n")
    |> Enum.map(fn(line) -> String.split(line, ":") end)
    |> Enum.map(fn([term | _]) -> term end)
    |> Enum.map(fn(term) -> {String.length(term), term} end)
    |> Enum.reduce(fn(current_item, longest_item) ->
      {cl, _} = current_item
      {ll, _} = longest_item
      if cl > ll, do: current_item, else: longest_item
    end)
    |> elem(1)
  end

  def find_longest_e(file \\ @data_file) do
    File.stream!(file)
    |> Stream.map(fn(line) -> String.split(line, ":") end)
    |> Stream.map(fn([term | _]) -> term end)
    |> Stream.map(fn(term) -> {String.length(term), term} end)
    |> Enum.reduce(fn(current_item, longest_item) ->
      {cl, _} = current_item
      {ll, _} = longest_item
      if cl > ll, do: current_item, else: longest_item
    end)
    |> elem(1)
  end


  def test_table_data() do
    ["row1", "row2", "row3", "row4", "row5", "row6", "row4"]
  end

  def make_table(data) do
    content = Stream.cycle(["white", "gray"])
    |> Stream.zip(data)
    |> Enum.map(fn({bg, row}) ->
      "<tr><td class='#{bg}'>#{row}</td></tr>"
    end)
    |> Enum.join("\n")
    "<table>" <> content <> "</table>"
  end


  def unfold_example() do
    unfolder = fn(i) -> {i, i + 5} end
    Stream.unfold(1, unfolder)
  end

  def fibb() do
    unfolder = fn({fib1, fib2}) ->
      curr_val = fib1
      new_state = {fib2, fib1 + fib2}
      {curr_val, new_state}
    end
    Stream.unfold({0,1}, unfolder)
  end
  # 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597,


end
