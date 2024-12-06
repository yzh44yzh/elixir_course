defmodule TestFixtures do
  alias WorkReport.Model.{Day, Month, Task, CategoryReport, DayReport, MonthReport}

  def single_model_list_fixture_1, do: [month_model_fixture_1()]
  def single_model_list_fixture_2, do: [month_model_fixture_3()]
  def plural_model_list_fixture_1, do: [month_model_fixture_1(), month_model_fixture_2()]

  # related to single_model_list_fixture_2 with params month: 1, day: 3
  def single_model_list_report_fixture_2_m1_d3,
    do: [day_report_fixture_3(), month_report_fixture_1()]

  def plural_model_list_report_fixture_1_m6_d5,
    do: [day_report_fixture_5(), month_report_fixture_6()]

  defp month_model_fixture_1 do
    %Month{
      number: 6,
      title: "June",
      days: [
        %Day{
          number: 5,
          title: "tue",
          tasks: [
            %Task{
              description: "Review Pull Requests",
              time_spent: 27,
              category: "DEV"
            },
            %Task{
              description: "Daily Meeting",
              time_spent: 15,
              category: "COMM"
            }
          ]
        },
        %Day{
          number: 6,
          title: "wed",
          tasks: [
            %Task{
              description: "Daily Meeting",
              time_spent: 31,
              category: "COMM"
            },
            %Task{
              description: "TASK-42 Read BA documents",
              time_spent: 15,
              category: "DOC"
            }
          ]
        }
      ]
    }
  end

  defp month_model_fixture_2 do
    %Month{
      number: 8,
      title: "August",
      days: [
        %Day{
          number: 10,
          title: "mon",
          tasks: [
            %Task{
              description: "Review Pull Requests",
              time_spent: 10,
              category: "DEV"
            },
            %Task{
              description: "Daily Meeting",
              time_spent: 20,
              category: "COMM"
            }
          ]
        },
        %Day{
          number: 12,
          title: "wed",
          tasks: [
            %Task{
              description: "Daily Meeting",
              time_spent: 10,
              category: "COMM"
            },
            %Task{
              description: "Read API documentation",
              time_spent: 40,
              category: "DOC"
            }
          ]
        }
      ]
    }
  end

  defp month_model_fixture_3 do
    %Month{
      number: 1,
      title: "January",
      days: [
        %Day{
          number: 3,
          title: "mon",
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
          ]
        },
        %Day{
          number: 10,
          title: "mon",
          tasks: [
            %Task{
              description: "Review Arabic Pull Requests",
              time_spent: 30,
              category: "DEV"
            },
            %Task{
              description: "Daily Meeting with indians",
              time_spent: 15,
              category: "COMM"
            },
            %Task{
              description: "Implement LLM shitty API",
              time_spent: 120,
              category: "DEV"
            },
            %Task{
              description: "Implement ASAP fix",
              time_spent: 20,
              category: "DEV"
            }
          ]
        }
      ]
    }
  end

  defp month_report_fixture_1 do
    %MonthReport{
      avg_time_spent: 302,
      categories: [
        %CategoryReport{title: "COMM", time_spent: 35},
        %CategoryReport{title: "DEV", time_spent: 510},
        %CategoryReport{title: "OPS", time_spent: 0},
        %CategoryReport{title: "DOC", time_spent: 60},
        %CategoryReport{title: "WS", time_spent: 0},
        %CategoryReport{title: "EDU", time_spent: 0}
      ],
      days_spent: 2,
      number: 1,
      title: "January",
      total_time_spent: 605
    }
  end

  defp month_report_fixture_6 do
    %MonthReport{
      avg_time_spent: 44,
      categories: [
        %CategoryReport{title: "COMM", time_spent: 46},
        %CategoryReport{title: "DEV", time_spent: 27},
        %CategoryReport{title: "OPS", time_spent: 0},
        %CategoryReport{title: "DOC", time_spent: 15},
        %CategoryReport{title: "WS", time_spent: 0},
        %CategoryReport{title: "EDU", time_spent: 0}
      ],
      days_spent: 2,
      number: 6,
      title: "June",
      total_time_spent: 88
    }
  end

  defp day_report_fixture_3 do
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

  defp day_report_fixture_5 do
    %DayReport{
      number: 5,
      tasks: [
        %Task{
          description: "Review Pull Requests",
          time_spent: 27,
          category: "DEV"
        },
        %Task{
          description: "Daily Meeting",
          time_spent: 15,
          category: "COMM"
        }
      ],
      title: "tue",
      total_time_spent: 42
    }
  end
end
