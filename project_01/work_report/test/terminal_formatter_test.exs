defmodule TerminalFormatterTest do
  use ExUnit.Case

  alias WorkReport.{
    TerminalFormatter,
    Model.DayReport,
    Model.CategoryReport,
    Model.MonthReport,
    Model.Task
  }

  def get_month_report_fixture() do
    %MonthReport{
      avg_time_spent: 302,
      categories: [
        %CategoryReport{title: "COMM", time_spent: 35},
        %CategoryReport{title: "DEV", time_spent: 510},
        %CategoryReport{title: "DOC", time_spent: 60}
      ],
      days_spent: 2,
      number: 5,
      title: "May",
      total_time_spent: 605
    }
  end

  def get_month_report_expected_formatting() do
    "Month: May\n - COMM: 35m\n - DEV: 8h 30m\n - DOC: 1h\n   Total: 10h 5m, Days: 2, Avg: 5h 2m"
  end

  def get_day_report_expected_formatting() do
    "Day: 3 mon\n - DEV: Implement search - 4h\n - COMM: Daily Meeting with indians - 20m\n - DEV: Implement endoint for auth - 1h 40m\n - DOC: Read API docs and manuals - 1h\n   Total: 7h"
  end

  def get_day_report_fixture() do
    %DayReport{
      number: 3,
      tasks: [
        %Task{
          description: "Implement search",
          time_spent: 240,
          category: "DEV"
        },
        %Task{
          description: "Daily Meeting with indians",
          time_spent: 20,
          category: "COMM"
        },
        %Task{
          description: "Implement endoint for auth",
          time_spent: 100,
          category: "DEV"
        },
        %Task{
          description: "Read API docs and manuals",
          time_spent: 60,
          category: "DOC"
        }
      ],
      title: "mon",
      total_time_spent: 420
    }
  end

  describe "format_report_list" do
    test "should format list" do
      month_report = get_month_report_fixture()
      day_report = get_day_report_fixture()

      assert TerminalFormatter.format_report_list([month_report, day_report]) ==
               ([get_month_report_expected_formatting(), get_day_report_expected_formatting()]
                |> Enum.join("\n\n")) <> "\n"
    end
  end

  describe "format_report" do
    test "should format month report" do
      report_model = get_month_report_fixture()

      assert TerminalFormatter.format_report(report_model) ==
               get_month_report_expected_formatting()
    end

    test "should format day report" do
      report_model = get_day_report_fixture()

      assert TerminalFormatter.format_report(report_model) == get_day_report_expected_formatting()
    end
  end

  describe "format report entities" do
    test "should format category report" do
      assert TerminalFormatter.format_category_report(%CategoryReport{
               title: "SOME",
               time_spent: 83
             }) ==
               "SOME: 1h 23m"
    end

    test "should format task" do
      %DayReport{tasks: tasks} = get_day_report_fixture()

      assert TerminalFormatter.format_task(tasks |> Enum.at(0)) == "DEV: Implement search - 4h"

      assert TerminalFormatter.format_task(tasks |> Enum.at(1)) ==
               "COMM: Daily Meeting with indians - 20m"

      assert TerminalFormatter.format_task(tasks |> Enum.at(2)) ==
               "DEV: Implement endoint for auth - 1h 40m"
    end
  end

  describe "format primitives" do
    test "should format the list of string" do
      assert TerminalFormatter.format_list(["some", "cool", "list"]) ==
               " - some\n - cool\n - list"
    end

    test "should format time from integer" do
      assert TerminalFormatter.format_time(2) == "2m"
      assert TerminalFormatter.format_time(60) == "1h"
      assert TerminalFormatter.format_time(65) == "1h 5m"
      assert TerminalFormatter.format_time(0) == "0"
    end
  end
end
