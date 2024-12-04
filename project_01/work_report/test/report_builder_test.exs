defmodule ReportBuilderTest do
  use ExUnit.Case

  alias WorkReport.Model.{CategoryReport, Day, Month, MonthReport, Task}
  alias WorkReport.ReportBuilder

  def get_month_model_fixture do
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

  describe "build_month_report" do
    test "should build month report" do
      assert ReportBuilder.build_month_report(get_month_model_fixture()) ==
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
  end
end
