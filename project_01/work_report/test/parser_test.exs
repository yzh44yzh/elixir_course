defmodule ParserTest do
  use ExUnit.Case

  alias WorkReport.Model.{DayReport, MonthReport, Task}
  alias WorkReport.Parser

  test "prepare file content" do
    str = """
    # March

    ## 09 tue
    [DEV] TASK-15 implement feature - 42m
    [COMM] Daily Meeting - 24m
    [DEV] TASK-15 implement - 25m

    ## 10 wed
    [DEV] Review Pull Requests - 17m
    [COMM] Sprint Planning - 1h
    # April

    ## 15 thu
    [COMM] Daily Meeting - 19m

    ## 16 fri
    [DEV] TASK-20 implementation - 17m
    [COMM] Daily Meeting - 22m
    [DEV] TASK-19 investigate bug - 43m
    """

    result = [
      "# March",
      "## 09 tue",
      "[DEV] TASK-15 implement feature - 42m",
      "[COMM] Daily Meeting - 24m",
      "[DEV] TASK-15 implement - 25m",
      "## 10 wed",
      "[DEV] Review Pull Requests - 17m",
      "[COMM] Sprint Planning - 1h",
      "# April",
      "## 15 thu",
      "[COMM] Daily Meeting - 19m",
      "## 16 fri",
      "[DEV] TASK-20 implementation - 17m",
      "[COMM] Daily Meeting - 22m",
      "[DEV] TASK-19 investigate bug - 43m"
    ]

    assert Parser.prepare_file_content(str) == result
  end

  test "group by marker" do
    data = [
      "## 09 tue",
      "[DEV] TASK-15 implement feature - 42m",
      "[COMM] Daily Meeting - 24m",
      "[DEV] TASK-15 implement - 25m",
      "## 10 wed",
      "[DEV] Review Pull Requests - 17m",
      "[COMM] Sprint Planning - 1h",
      "## 15 thu",
      "[COMM] Daily Meeting - 19m",
      "## 16 fri",
      "[DEV] TASK-20 implementation - 17m",
      "[COMM] Daily Meeting - 22m",
      "[DEV] TASK-19 investigate bug - 43m"
    ]

    result = [
      %{
        header: "09 tue",
        items: [
          "[DEV] TASK-15 implement feature - 42m",
          "[COMM] Daily Meeting - 24m",
          "[DEV] TASK-15 implement - 25m"
        ]
      },
      %{
        header: "10 wed",
        items: [
          "[DEV] Review Pull Requests - 17m",
          "[COMM] Sprint Planning - 1h"
        ]
      },
      %{
        header: "15 thu",
        items: [
          "[COMM] Daily Meeting - 19m"
        ]
      },
      %{
        header: "16 fri",
        items: [
          "[DEV] TASK-20 implementation - 17m",
          "[COMM] Daily Meeting - 22m",
          "[DEV] TASK-19 investigate bug - 43m"
        ]
      }
    ]

    assert Parser.group_by_marker(data, "## ") == result
  end

  test "trim prefix" do
    assert Parser.trim_prefix("# May", "# ") == "May"
    assert Parser.trim_prefix("## 16 fri", "## ") == "16 fri"
    assert Parser.trim_prefix("some prefix for some str", "some prefix ") == "for some str"
  end

  test "get category" do
    assert Parser.get_category("[DEV] Task 1") == {"DEV", "Task 1"}
    assert Parser.get_category("[COMM] Daily Meeting") == {"COMM", "Daily Meeting"}
    assert Parser.get_category("[SOME] some") == :not_found
  end

  test "split to months and days" do
    data = [
      "# March",
      "## 09 tue",
      "[DEV] TASK-15 implement feature - 42m",
      "[COMM] Daily Meeting - 24m",
      "[DEV] TASK-15 implement - 25m",
      "## 10 wed",
      "[DEV] Review Pull Requests - 17m",
      "[COMM] Sprint Planning - 1h",
      "# April",
      "## 15 thu",
      "[COMM] Daily Meeting - 19m",
      "## 16 fri",
      "[DEV] TASK-20 implementation - 17m",
      "[COMM] Daily Meeting - 22m",
      "[DEV] TASK-19 investigate bug - 43m"
    ]

    result = [
      %{
        header: "March",
        items: [
          %{
            header: "09 tue",
            items: [
              "[DEV] TASK-15 implement feature - 42m",
              "[COMM] Daily Meeting - 24m",
              "[DEV] TASK-15 implement - 25m"
            ]
          },
          %{
            header: "10 wed",
            items: [
              "[DEV] Review Pull Requests - 17m",
              "[COMM] Sprint Planning - 1h"
            ]
          }
        ]
      },
      %{
        header: "April",
        items: [
          %{
            header: "15 thu",
            items: [
              "[COMM] Daily Meeting - 19m"
            ]
          },
          %{
            header: "16 fri",
            items: [
              "[DEV] TASK-20 implementation - 17m",
              "[COMM] Daily Meeting - 22m",
              "[DEV] TASK-19 investigate bug - 43m"
            ]
          }
        ]
      }
    ]

    assert Parser.split_to_months_and_days(data) == result
  end

  test "map to model" do
    data = [
      %{
        header: "March",
        items: [
          %{
            header: "10 wed",
            items: [
              "[DEV] Review Pull Requests - 17m",
              "[COMM] Sprint Planning - 1h"
            ]
          }
        ]
      },
      %{
        header: "April",
        items: [
          %{
            header: "15 thu",
            items: [
              "[COMM] Daily Meeting - 19m"
            ]
          },
          %{
            header: "16 fri",
            items: [
              "[COMM] Daily Meeting - 22m",
              "[DEV] TASK-19 investigate bug - 43m"
            ]
          }
        ]
      }
    ]

    result = [
      %MonthReport{
        month: "March",
        month_num: 3,
        days: [
          %DayReport{
            day: "10 wed",
            day_num: 10,
            tasks: [
              %Task{category: "DEV", description: "Review Pull Requests", time: 17},
              %Task{category: "COMM", description: "Sprint Planning", time: 60}
            ]
          }
        ]
      },
      %MonthReport{
        month: "April",
        month_num: 4,
        days: [
          %DayReport{
            day: "15 thu",
            day_num: 15,
            tasks: [
              %Task{category: "COMM", description: "Daily Meeting", time: 19}
            ]
          },
          %DayReport{
            day: "16 fri",
            day_num: 16,
            tasks: [
              %Task{category: "COMM", description: "Daily Meeting", time: 22},
              %Task{category: "DEV", description: "TASK-19 investigate bug", time: 43}
            ]
          }
        ]
      }
    ]

    assert Parser.map_to_model(data) == result
  end

  test "parse task" do
    data = "[DEV] TASK-20 implementation - 17m"
    result = %Task{category: "DEV", description: "TASK-20 implementation", time: 17}
    assert Parser.parse_task(data) == result
  end

  test "parse time" do
    assert Parser.parse_time("1m") == 1
    assert Parser.parse_time("5m") == 5
    assert Parser.parse_time("12m") == 12
    assert Parser.parse_time("42m") == 42
    assert Parser.parse_time("59m") == 59
    assert Parser.parse_time("60m") == 60
    assert Parser.parse_time("61m") == 61
    assert Parser.parse_time("1h") == 60
    assert Parser.parse_time("1h 5m") == 65
    assert Parser.parse_time("1h 30m") == 90
    assert Parser.parse_time("2h 20m") == 140
    assert Parser.parse_time("1h 90m") == 150
    assert Parser.parse_time("3h") == 180
    assert Parser.parse_time("10h") == 600
    assert Parser.parse_time("10h 15m") == 615

    assert Parser.parse_time("") == 0
    assert Parser.parse_time("0m") == 0
    assert Parser.parse_time("0h") == 0
    assert Parser.parse_time("0m 0h") == 0
    assert Parser.parse_time("whatever") == 0
  end
end
