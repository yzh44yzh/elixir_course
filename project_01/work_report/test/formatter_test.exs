defmodule FormatterTest do
  use ExUnit.Case

  alias WorkReport.Formatter
  alias WorkReport.Model.{DayReport, MonthReport, Task}

  test "format time" do
    assert Formatter.format_time(0) == "0"
    assert Formatter.format_time(1) == "1m"
    assert Formatter.format_time(12) == "12m"
    assert Formatter.format_time(42) == "42m"
    assert Formatter.format_time(59) == "59m"
    assert Formatter.format_time(60) == "1h"
    assert Formatter.format_time(61) == "1h 1m"
    assert Formatter.format_time(65) == "1h 5m"
    assert Formatter.format_time(90) == "1h 30m"
    assert Formatter.format_time(140) == "2h 20m"
    assert Formatter.format_time(150) == "2h 30m"
    assert Formatter.format_time(180) == "3h"
    assert Formatter.format_time(600) == "10h"
    assert Formatter.format_time(615) == "10h 15m"
  end

  test "format category stat" do
    stat = %{
      "DEV" => 120,
      "DOC" => 64,
      "WS" => 12,
      "EDU" => 127
    }

    result = [
      " - COMM: 0\n",
      " - DEV: 2h\n",
      " - OPS: 0\n",
      " - DOC: 1h 4m\n",
      " - WS: 12m\n",
      " - EDU: 2h 7m\n"
    ]

    assert Formatter.format_category_stat(stat) == result
  end

  test "format task" do
    task = %Task{
      category: "DEV",
      description: "TASK-19 investigate bug",
      time: 43
    }

    result = " - DEV: TASK-19 investigate bug - 43m\n"

    assert Formatter.format_task(task) == result
  end

  test "format day" do
    report = %DayReport{
      day: "16 fri",
      day_num: 16,
      tasks: [
        %Task{category: "COMM", description: "Daily Meeting", time: 22},
        %Task{category: "DEV", description: "TASK-19 investigate bug", time: 43}
      ]
    }

    result = [
      "Day: 16 fri\n",
      [
        " - COMM: Daily Meeting - 22m\n",
        " - DEV: TASK-19 investigate bug - 43m\n"
      ],
      "   Total: 1h 5m\n"
    ]

    assert Formatter.format_day(report) == result
  end

  test "format month" do
    report = %MonthReport{
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

    result = [
      "Month: April\n",
      [
        " - COMM: 41m\n",
        " - DEV: 43m\n",
        " - OPS: 0\n",
        " - DOC: 0\n",
        " - WS: 0\n",
        " - EDU: 0\n"
      ],
      "   Total: 1h 24m, Days: 2, Avg: 42m\n"
    ]

    assert Formatter.format_month(report) == result
  end
end
