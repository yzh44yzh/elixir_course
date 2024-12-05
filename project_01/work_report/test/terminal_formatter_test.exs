defmodule TerminalFormatterTest do
  use ExUnit.Case

  alias WorkReport.{TerminalFormatter, Model.MonthReport, Model.CategoryReport}

  def get_month_report_fixture() do
    %MonthReport{
      avg_time_spent: 302,
      categories: [
        %CategoryReport{title: "COMM", time_spent: 35},
        %CategoryReport{title: "DEV", time_spent: 510},
        %CategoryReport{title: "DOC", time_spent: 60}
      ],
      days_spent: 2,
      total_time_spent: 605
    }
  end

  test "should format report" do
    report_model = get_month_report_fixture()

    assert TerminalFormatter.format_report(report_model) ==
             "Month: May\n - COMM: 35m\n - DEV: 8h 30m\n - DOC: 1h\n   Total: 10h 5m, Days: 2, Avg: 5h 2m"
  end
end
