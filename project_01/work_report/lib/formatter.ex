defmodule WorkReport.Formatter do
  alias WorkReport.Model
  alias WorkReport.Model.{DayReport, MonthReport, Task}
  alias WorkReport.Stat

  @spec format_month(MonthReport.t()) :: IO.chardata()
  def format_month(report) do
    total_time = Stat.total_time(report)
    num_days = length(report.days)
    avg_time = div(total_time, num_days) |> format_time()
    total_time = format_time(total_time)

    [
      "Month: #{report.month}\n",
      Stat.category_stat(report) |> format_category_stat(),
      "   Total: #{total_time}, Days: #{num_days}, Avg: #{avg_time}\n"
    ]
  end

  @spec format_day(DayReport.t()) :: IO.chardata()
  def format_day(report) do
    time = Stat.total_time(report) |> format_time

    [
      "Day: #{report.day}\n",
      Enum.map(report.tasks, &format_task/1),
      "   Total: #{time}\n"
    ]
  end

  @spec format_task(Task.t()) :: String.t()
  def format_task(task) do
    time = format_time(task.time)
    " - #{task.category}: #{task.description} - #{time}\n"
  end

  @spec format_time(integer) :: String.t()
  def format_time(time) do
    hours = div(time, 60)
    mins = rem(time, 60)

    case {hours, mins} do
      {0, 0} -> "0"
      {h, 0} -> "#{h}h"
      {0, m} -> "#{m}m"
      {h, m} -> "#{h}h #{m}m"
    end
  end

  @spec format_category_stat(Stat.category_stat()) :: IO.chardata()
  def format_category_stat(stat) do
    Enum.map(Model.categories(), fn category ->
      time = Map.get(stat, category, 0) |> format_time()
      " - #{category}: #{time}\n"
    end)
  end
end
