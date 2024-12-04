defmodule WorkReport.ReportBuilder do
  @moduledoc """
    Report model

    // MarkdownParser module implements behaviour from Parser module.
    report.md -> Parser.parse_report(report_file, MarkdownParser) -> %Month{}

    %Month{} -> build_report(day: 5) -> {_, %DayReport{}} // %Report{month: nil, day: %DayReport{}}
    %Month{} -> build_report(month: 2) -> {%MonthReport{}, _} // %Report{month: %MonthReport{}, day: nil}
    %Month{} -> build_report(month: 2, day: 5) -> {%MonthReport{}, %DayReport{}} // %Report{month: %MonthReport{}, day: %DayReport{}}

    // TerminalPrinter module implements behaviour from Printer module.
    %Report{} -> Printer.print_report(%Report{}, TerminalPrinter)
  """

  alias WorkReport.Model.{CategoryReport, Day, DayReport, Month, MonthReport, Task}

  defmodule MonthNotFoundError do
    defexception [:message]

    @impl true
    def exception(month_number) do
      %MonthNotFoundError{message: "Month number #{inspect(month_number)} was not found!"}
    end
  end

  defmodule DayNotFoundError do
    defexception [:message]

    @impl true
    def exception(day_number) do
      %DayNotFoundError{message: "Day number #{inspect(day_number)} was not found!"}
    end
  end

  @spec build_report(month :: Month.t(), month_number :: integer(), day_number :: integer()) ::
          {MonthReport.t(), DayReport.t()}
  def build_report(month, month_number, day_number) when month.number == month_number do
    {build_month_report(month), build_day_report(month, day_number)}
  end

  def build_report(month, month_number, day_number) do
    raise MonthNotFoundError, month_number
  end

  @spec build_day_report(month :: Month.t(), day_number :: integer()) ::
          DayReport.t() | {:error, String.t()}
  def build_day_report(%Month{days: days}, day_number) do
    case Enum.find(days, fn %Day{number: number} -> number == day_number end) do
      nil ->
        raise DayNotFoundError, day_number

      %Day{tasks: tasks} ->
        %DayReport{total_time_spent: count_tasks_time_spent(tasks), tasks: tasks}
    end
  end

  @spec count_tasks_time_spent(tasks :: [Task.t()]) :: integer()
  def count_tasks_time_spent(tasks) do
    tasks
    |> Stream.map(fn %Task{time_spent: time_spent} -> time_spent end)
    |> Enum.sum()
  end

  def build_month_report(%Month{days: days}) do
    days_spent = length(days)

    tasks =
      days
      |> Enum.flat_map(fn %Day{tasks: tasks} -> tasks end)

    total_time_spent = count_tasks_time_spent(tasks)

    avg_time_spent = trunc(total_time_spent / days_spent)

    categories =
      tasks
      |> Enum.group_by(
        fn %Task{category: category} -> category end,
        fn %Task{time_spent: time_spent} -> time_spent end
      )
      |> Map.to_list()
      |> Enum.map(fn {category, time_spent_values} ->
        %CategoryReport{title: category, time_spent: Enum.sum(time_spent_values)}
      end)

    %MonthReport{
      avg_time_spent: avg_time_spent,
      categories: categories,
      days_spent: days_spent,
      total_time_spent: total_time_spent
    }
  end
end
