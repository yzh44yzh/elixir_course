defmodule Report do
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

  defmodule Month do
    @moduledoc """
    Month model:
    - title :: string
    - id :: int()
    - days :: list(%Day{})
    - getMonthReport(%Month{}, day_number :: int) ::
      %MonthReport{
        categories: %Category{title :: string, total :: int()},
        total_time_spent: int(),
        days_count :: int(),
        avg_time_spent :: int()
      }
    """
  end

  defmodule MonthReport do
    @moduledoc """
    Month report model:
      categories: %Category{title :: string, total :: int()},
      total_time_spent: int(),
      days_count :: int(),
      avg_time_spent :: int()
    """
  end

  defmodule Day do
    @moduledoc """
    Day model:
    - title :: string
    - id :: int()
    - tasks :: list(%Task{})
    - getDayReport(%Day{}) ::
      %DayReport{
        tasks: [%Task{}],
        total_time_spent :: int()
      }
    """
  end

  defmodule DayReport do
    @moduledoc """
    Day report model:
      tasks: [%Task{}],
      total_time_spent :: int()
    """
  end

  defmodule Task do
    @moduledoc """
    Task model:
      - category :: string() (%Category{} ???)
      - description :: string()
      - time_spent :: int()
    """
  end
end
