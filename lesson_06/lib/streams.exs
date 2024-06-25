defmodule StreamExamples do
  def enum_example() do
    list = [1, 2, 3, 4, 5]

    Enum.map(list, fn i -> i * 2 end)
    |> Enum.zip([:a, :b, :c, :d, :e])
    |> Enum.filter(fn {a, _b} -> a > 2 end)
  end

  def stream_example() do
    list = [1, 2, 3, 4, 5]

    Stream.map(list, fn i -> i * 2 end)
    |> Stream.zip([:a, :b, :c, :d, :e])
    |> Stream.filter(fn {a, _b} -> a > 2 end)
  end

  @data_file "./data/dictionary.txt"

  def find_longest(file \\ @data_file) do
    File.read!(file)
    |> String.split("\n")
    |> Enum.map(fn line -> String.split(line, ":") end)
    |> Enum.map(fn [term | _] -> term end)
    |> Enum.map(fn term -> {String.length(term), term} end)
    |> Enum.max_by(fn {len, _term} -> len end)
    |> elem(1)
  end

  def find_longest_lazy(file \\ @data_file) do
    File.stream!(file)
    |> Stream.map(fn line -> String.split(line, ":") end)
    |> Stream.map(fn [term | _] -> term end)
    |> Stream.map(fn term -> {String.length(term), term} end)
    |> Enum.max_by(fn {len, _term} -> len end)
    |> elem(1)
  end

  def test_table_data() do
    ["row1", "row2", "row3", "row4", "row5", "row6", "row4"]
  end

  def make_table(data) do
    content =
      Stream.cycle(["white", "gray"])
      |> Stream.zip(data)
      |> Enum.map(fn {bg, row} ->
        "<tr><td class='#{bg}'>#{row}</td></tr>"
      end)
      |> Enum.join("\n")

    "<table>" <> content <> "</table>"
  end

  def test_data do
    [
      {"Bob", 24},
      {"Bill", 25},
      {"Kate", 26},
      {"Helen", 34},
      {"Yury", 16}
    ]
  end

  def make_table_2(data) do
    css_styles = Stream.cycle(["white", "gray"])
    iterator = Stream.iterate(1, fn i -> i + 1 end)

    rows =
      Stream.zip(css_styles, iterator)
      |> Stream.zip(data)
      |> Enum.map(fn {{css_style, index}, {name, age}} ->
        "<tr class='#{css_style}'><td>#{index}</td><td>#{name}</td><td>#{age}</td></tr>"
      end)
      |> Enum.join("\n")

    "<table>#{rows}</table>"
  end

  def make_table_3(users) do
    initial_state = {true, 1}

    unfolder = fn {odd, index} ->
      value = %{odd: odd, index: index}
      new_state = {not odd, index + 1}
      {value, new_state}
    end

    rows =
      Stream.unfold(initial_state, unfolder)
      |> Stream.zip(users)
      |> Enum.map(fn {state, user} ->
        css_style = if state.odd, do: "white", else: "gray"
        {name, age} = user
        "<tr class='#{css_style}'><td>#{state.index}</td><td>#{name}</td><td>#{age}</td></tr>"
      end)
      |> Enum.join("\n")

    "<table>#{rows}</table>"
  end

  def fibb() do
    unfolder = fn {fib1, fib2} ->
      curr_val = fib1
      new_state = {fib2, fib1 + fib2}
      {curr_val, new_state}
    end

    Stream.unfold({0, 1}, unfolder)
  end

  # 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597,

end
