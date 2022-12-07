defmodule WorkReport.Parser do
  alias WorkReport.Model
  alias WorkReport.Model.{DayReport, MonthReport, Task}

  def parse(file) do
    File.read!(file)
    |> prepare_file_content()
    |> split_to_months_and_days()
    |> map_to_model()
  end

  def prepare_file_content(file_content) do
    file_content
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.filter(fn str -> str != "" end)
  end

  def group_by_marker(data, marker) do
    [first_line | rest] = data

    if not String.starts_with?(first_line, marker),
      do: raise("data should start from marker '#{marker}'")

    first_line = trim_prefix(first_line, marker)
    first_group = %{header: first_line, items: []}
    acc = {first_group, []}

    {last_group, groups} =
      Enum.reduce(rest, acc, fn line, {curr_group, groups} ->
        if String.starts_with?(line, marker) do
          line = trim_prefix(line, marker)
          new_group = %{header: line, items: []}
          groups = [curr_group | groups]
          {new_group, groups}
        else
          curr_group = update_in(curr_group.items, fn items -> [line | items] end)
          {curr_group, groups}
        end
      end)

    [last_group | groups]
    |> Enum.map(fn group -> %{group | items: Enum.reverse(group.items)} end)
    |> Enum.reverse()
  end

  def trim_prefix(str, prefix) do
    String.slice(str, String.length(prefix), String.length(str))
  end

  def split_to_months_and_days(data) do
    group_by_marker(data, "# ")
    |> Enum.map(&split_month_to_days/1)
  end

  def split_month_to_days(%{header: header, items: items}) do
    days = group_by_marker(items, "## ")
    %{header: header, items: days}
  end

  def get_category(data) do
    Enum.reduce(
      Model.categories(),
      :not_found,
      fn
        category, :not_found ->
          prefix = "[#{category}] "

          if String.starts_with?(data, prefix) do
            {category, trim_prefix(data, prefix)}
          else
            :not_found
          end

        _, found ->
          found
      end
    )
  end

  def map_to_model(map) do
    Enum.map(map, &month_to_model/1)
  end

  def month_to_model(%{header: header, items: days}) do
    days = Enum.map(days, &day_to_model/1)
    MonthReport.new(header, days)
  end

  def day_to_model(%{header: header, items: tasks}) do
    tasks = Enum.map(tasks, &parse_task/1)
    DayReport.new(header, tasks)
  end

  def parse_task(data) do
    {category, rest} = get_category(data)
    [description, time_str] = String.split(rest, " - ")
    time = parse_time(time_str)
    Task.new(category, description, time)
  end

  @spec parse_time(String.t()) :: integer
  def parse_time(time_str) do
    time_str
    |> String.split(" ")
    |> Enum.map(&parse_time_item/1)
    |> Enum.sum()
  end

  def parse_time_item(item) do
    case Integer.parse(item) do
      {i, "h"} -> i * 60
      {i, "m"} -> i
      _ -> 0
    end
  end
end
