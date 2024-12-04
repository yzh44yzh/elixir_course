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

  @spec build_report(month :: Month.t(), month_number :: integer(), day_number :: integer()) ::
          {MonthReport.t(), DayReport.t()}
  def build_report(month, month_number, day_number) do
    month
  end

  # avg_time_spent: integer(),
  # categories: [CategoryReport.t()],
  # days_spent: integer(),
  # total_time_spent: integer()
  @spec build_month_report(month :: Month) :: MonthReport.t()
  def build_month_report(%Month{days: days}) do
    days_spent = length(days)

    tasks =
      days
      |> Enum.flat_map(fn %Day{tasks: tasks} -> tasks end)

    total_time_spent =
      tasks
      |> Stream.map(fn %Task{time_spent: time_spent} -> time_spent end)
      |> Enum.sum()

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
