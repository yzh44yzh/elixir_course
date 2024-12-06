defmodule WorkReport.ReportBuilder do
  @moduledoc """
    Module takes models of subject area and returns list of report models.
  """

  alias WorkReport.Model.{CategoryReport, Day, DayReport, Month, MonthReport, Task, Report}

  defmodule MonthNotFoundError do
    defexception [:message]

    @impl true
    def exception(month_number) do
      %MonthNotFoundError{message: "month #{inspect(month_number)} not found"}
    end
  end

  defmodule DayNotFoundError do
    defexception [:message]

    @impl true
    def exception(day_number) do
      %DayNotFoundError{message: "day #{inspect(day_number)} not found"}
    end
  end

  @spec build_report(
          month_list :: [Month.t()],
          month_number :: integer(),
          day_number :: integer()
        ) ::
          [Report.t()]
  def build_report(month_list, month_number, day_number) do
    case Enum.find(month_list, fn %Month{number: number} -> number == month_number end) do
      nil ->
        raise MonthNotFoundError, month_number

      %Month{} = month ->
        [build_day_report(month, day_number), build_month_report(month)]
    end
  end

  @spec build_day_report(month :: Month.t(), day_number :: integer()) ::
          DayReport.t() | {:error, String.t()}
  def build_day_report(%Month{days: days}, day_number) do
    case Enum.find(days, fn %Day{number: number} -> number == day_number end) do
      nil ->
        raise DayNotFoundError, day_number

      %Day{number: number, tasks: tasks, title: title} ->
        %DayReport{
          number: number,
          tasks: tasks,
          title: title,
          total_time_spent: count_tasks_time_spent(tasks)
        }
    end
  end

  @spec count_tasks_time_spent(tasks :: [Task.t()]) :: integer()
  def count_tasks_time_spent(tasks) do
    tasks
    |> Stream.map(fn %Task{time_spent: time_spent} -> time_spent end)
    |> Enum.sum()
  end

  @spec build_month_report(month :: Month.t()) :: MonthReport.t()
  def build_month_report(%Month{days: days, number: number, title: title}) do
    days_spent = length(days)

    tasks =
      days
      |> Enum.flat_map(fn %Day{tasks: tasks} -> tasks end)

    total_time_spent = count_tasks_time_spent(tasks)

    avg_time_spent = trunc(total_time_spent / days_spent)

    categories =
      tasks
      |> Enum.reduce(
        [
          {:COMM, 0},
          {:DEV, 0},
          {:OPS, 0},
          {:DOC, 0},
          {:WS, 0},
          {:EDU, 0}
        ],
        fn %Task{category: category, time_spent: time_spent}, acc ->
          Keyword.update(acc, String.to_atom(category), time_spent, fn count ->
            count + time_spent
          end)
        end
      )
      |> Enum.map(fn {category, time_spent_value} ->
        %CategoryReport{title: Atom.to_string(category), time_spent: time_spent_value}
      end)

    %MonthReport{
      avg_time_spent: avg_time_spent,
      categories: categories,
      days_spent: days_spent,
      number: number,
      title: title,
      total_time_spent: total_time_spent
    }
  end
end
