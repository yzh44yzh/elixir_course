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

    context = %{day_10: day_10, day_16: day_16}
    {:ok, context}
  end

  test "day total time", context do
    assert Stat.total_time(context.day_10) == 77
    assert Stat.total_time(context.day_16) == 65
  end

  test "month total time", context do
    report = %MonthReport{
      month: "April",
      month_num: 4,
      days: [context.day_10, context.day_16]
    }

    assert Stat.total_time(report) == 142
  end
end
