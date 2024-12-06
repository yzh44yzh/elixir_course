defmodule WorkReport.TerminalFormatter do
  @moduledoc """
  Module is a terminal specific implementation of the formatter.
  It takes the list of report models and returns binary for printing full report.
  """
  alias WorkReport.{
    Formatter,
    Model.CategoryReport,
    Model.DayReport,
    Model.MonthReport,
    Model.Task
  }

  @minutes_in_one_hour 60

  @behaviour Formatter

  @impl Formatter
  def format_report_list(report_list, _opts \\ []) do
    (report_list |> Stream.map(&format_report/1) |> Enum.join("\n\n")) <> "\n"
  end

  @spec format_report(report :: MonthReport.t()) :: binary()
  def format_report(%MonthReport{
        avg_time_spent: avg_time_spent,
        categories: categories,
        days_spent: days_spent,
        title: title,
        total_time_spent: total_time_spent
      }) do
    report = [
      "Month: #{title}",
      categories |> Enum.map(&format_category_report/1) |> format_list(),
      "\s\s\s" <>
        "Total: #{format_time(total_time_spent)}, " <>
        "Days: #{days_spent}, " <>
        "Avg: #{format_time(avg_time_spent)}"
    ]

    Enum.join(report, "\n")
  end

  @spec format_report(report :: DayReport.t()) :: binary()
  def format_report(%DayReport{
        number: number,
        tasks: tasks,
        title: title,
        total_time_spent: total_time_spent
      }) do
    report = [
      "Day: #{number} #{title}",
      tasks |> Enum.map(&format_task/1) |> format_list(),
      "\s\s\s" <>
        "Total: #{format_time(total_time_spent)}"
    ]

    Enum.join(report, "\n")
  end

  @spec format_list(list :: [String.t()]) :: binary()
  def format_list(list) do
    list |> Stream.map(fn element -> " - #{element}" end) |> Enum.join("\n")
  end

  @spec format_category_report(category_report :: CategoryReport.t()) :: binary()
  def format_category_report(%CategoryReport{title: title, time_spent: time_spent}) do
    "#{title}: #{format_time(time_spent)}"
  end

  @spec format_task(task :: Task.t()) :: binary()
  def format_task(%Task{category: category, description: description, time_spent: time_spent}) do
    "#{category}: #{description} - #{format_time(time_spent)}"
  end

  @spec format_time(time :: integer()) :: String.t()
  def format_time(time) do
    hours = div(time, @minutes_in_one_hour)
    minutes = rem(time, @minutes_in_one_hour)

    case {hours, minutes} do
      {0, 0} -> "0"
      {0, minutes} -> "#{minutes}m"
      {hours, 0} -> "#{hours}h"
      {hours, minutes} -> "#{hours}h #{minutes}m"
    end
  end
end
