defmodule WorkReport.Stat do
  alias WorkReport.Model, as: M

  @type category_stat() :: Map.t(String.t(), integer)

  @spec day_stat([M.Task.t()]) :: integer
  def day_stat(tasks) do
    tasks
    |> Enum.map(fn task -> task.time end)
    |> Enum.sum()
  end

  @spec month_stat(M.DayReport.t()) :: integer
  def month_stat(days) do
    days
    |> Enum.map(fn day -> day_stat(day.tasks) end)
    |> Enum.sum()
  end

  @spec category_stat_per_day(M.DayReport.t()) :: category_stat
  def category_stat_per_day(report) do
    Enum.reduce(report.tasks, %{}, fn task, acc ->
      Map.update(acc, task.category, task.time, fn old_val -> old_val + task.time end)
    end)
  end

  @spec category_stat_per_month(M.MonthReport.t()) :: category_stat
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
