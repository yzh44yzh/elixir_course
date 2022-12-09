defmodule WorkReport.Stat do
  alias WorkReport.Model.{DayReport, MonthReport}

  @type category_stat() :: Map.t(String.t(), integer())

  @spec total_time(MonthReport.t() | DayReport.t()) :: integer()
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

  @spec category_stat(MonthReport.t() | DayReport.t()) :: category_stat()
  def category_stat(%MonthReport{days: days}) do
    Enum.reduce(days, %{}, fn day, acc ->
      Map.merge(
        acc,
        category_stat(day),
        fn _category, acc_val, day_val ->
          acc_val + day_val
        end
      )
    end)
  end

  def category_stat(%DayReport{tasks: tasks}) do
    Enum.reduce(tasks, %{}, fn task, acc ->
      Map.update(
        acc,
        task.category,
        task.time,
        fn time -> time + task.time end
      )
    end)
  end
end
