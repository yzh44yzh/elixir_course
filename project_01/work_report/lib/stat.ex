defmodule WorkReport.Stat do
  alias WorkReport.Model.{DayReport, MonthReport}

  @type category_stat() :: Map.t(String.t(), integer)

  @spec total_time(MonthReport.t() | DayReport.t()) :: integer
  def total_time(%MonthReport{days: days}) do
    days
    |> Enum.map(&total_time/1)
    |> Enum.sum()
  end

  def total_time(%DayReport{tasks: tasks}) do
    tasks
    |> Enum.map(fn task -> task.time end)
    |> Enum.sum()
  end

  @spec category_stat_per_day(DayReport.t()) :: category_stat
  def category_stat_per_day(report) do
    Enum.reduce(report.tasks, %{}, fn task, acc ->
      Map.update(acc, task.category, task.time, fn old_val -> old_val + task.time end)
    end)
  end

  @spec category_stat_per_month(MonthReport.t()) :: category_stat
  def category_stat_per_month(report) do
    Enum.reduce(report.days, %{}, fn day, acc ->
      Map.merge(
        acc,
        category_stat_per_day(day),
        fn _category, acc_val, day_val ->
          acc_val + day_val
        end
      )
    end)
  end
end
