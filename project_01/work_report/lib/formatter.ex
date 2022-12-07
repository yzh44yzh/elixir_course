defmodule WorkReport.Formatter do
  alias WorkReport.Model, as: M
  alias WorkReport.Stat

  @spec format_month(M.MonthReport.t()) :: IO.chardata()
  def format_month(report) do
    total_time = Stat.month_stat(report.days)
    num_days = length(report.days)

    [
      "Month: ",
      report.month,
      "\n",
      Stat.category_stat_per_month(report) |> format_category_stat(),
      "   Total: ",
      total_time |> format_time,
      ", Days: ",
      num_days |> to_string,
      ", Avg: ",
      div(total_time, num_days) |> format_time
    ]
  end

  @spec format_day(M.DayReport.t()) :: IO.chardata()
  def format_day(report) do
    [
      "Day: #{report.day}\n",
      Enum.map(report.tasks, &format_task/1),
      "   Total: ",
      Stat.day_stat(report.tasks) |> format_time
    ]
  end

  @spec format_task(M.Task.t()) :: IO.chardata()
  def format_task(task) do
    [
      " - ",
      task.category,
      ": ",
      task.description,
      " - ",
      format_time(task.time),
      "\n"
    ]
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

  @spec format_category_stat(WrStat.category_stat()) :: IO.chardata()
  def format_category_stat(stat) do
    Enum.map(M.categories(), fn category ->
      [
        " - #{category}: ",
        Map.get(stat, category, 0) |> format_time(),
        "\n"
      ]
    end)
  end
end
