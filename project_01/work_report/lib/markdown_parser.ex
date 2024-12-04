defmodule WorkReport.MarkdownParser do
  alias WorkReport.Model.Task
  @behaviour WorkReport.Parser

  @minutes_in_one_hour 60

  @impl WorkReport.Parser
  def parse_report(report, opts) do
    [month | days] =
      report
      |> String.split("\n\n")

    days
    |> Enum.map(fn day -> String.split(day, "\n") end)
    |> Enum.map(fn [day | tasks] ->
      [
        day |> String.split(" "),
        tasks
        |> Enum.map(&parse_task/1)
        |> Enum.reject(&is_nil/1)
      ]
    end)
  end

  @spec parse_task(task :: binary()) :: Task.t()
  def parse_task(task) do
    # Regexp to parse task string format: [Type] Some title - 1h 30m
    regexp =
      ~r/^\[(?<category>\w+)\]\s(?<description>.+)\s\-\s(?<time_spent>(\d{0,2}h)?\s?(\d{0,2}m)?)$/

    case Regex.named_captures(regexp, task) do
      nil ->
        nil

      %{"category" => category, "description" => description, "time_spent" => time_spent} ->
        %Task{category: category, description: description, time_spent: parse_time(time_spent)}
    end
  end

  @spec parse_time(String.t()) :: integer()
  def parse_time(time_string) do
    %{"hours" => hours, "minutes" => minutes} =
      Regex.named_captures(~r/^((?<hours>\d{0,2})h)?\s?((?<minutes>\d{0,2})m)?$/, time_string)

    string_to_int(hours) * @minutes_in_one_hour + string_to_int(minutes)
  end

  def string_to_int("") do
    0
  end

  def string_to_int(str) do
    String.to_integer(str)
  end
end
