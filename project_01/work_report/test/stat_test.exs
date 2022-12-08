defmodule StatTest do
  use ExUnit.Case

  alias WorkReport.Model.{DayReport, MonthReport, Task}
  alias WorkReport.Stat

  setup do
    day_10 = %DayReport{
      day: "10 wed",
      day_num: 10,
      tasks: [
        %Task{category: "DEV", description: "Review Pull Requests", time: 17},
        %Task{category: "COMM", description: "Sprint Planning", time: 60}
      ]
    }

    day_16 = %DayReport{
      day: "16 fri",
      day_num: 16,
      tasks: [
        %Task{category: "COMM", description: "Daily Meeting", time: 22},
        %Task{category: "DEV", description: "TASK-19 investigate bug", time: 43}
      ]
    }

    month_report = %MonthReport{
      month: "April",
      month_num: 4,
      days: [day_10, day_16]
    }

    context = %{
      day_10: day_10,
      day_16: day_16,
      month_report: month_report
    }

    {:ok, context}
  end

  test "total time for day", context do
    assert Stat.total_time(context.day_10) == 77
    assert Stat.total_time(context.day_16) == 65
  end

  test "total time for month", context do
    assert Stat.total_time(context.month_report) == 142
  end

  test "category stat for day", context do
    assert Stat.category_stat(context.day_10) == %{
             "DEV" => 17,
             "COMM" => 60
           }

    assert Stat.category_stat(context.day_16) == %{
             "DEV" => 43,
             "COMM" => 22
           }
  end

  test "category stat for month", context do
    assert Stat.category_stat(context.month_report) == %{
             "DEV" => 60,
             "COMM" => 82
           }
  end
end
