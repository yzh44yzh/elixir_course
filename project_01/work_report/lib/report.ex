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
end
