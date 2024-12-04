defmodule ReportBuilderTestFixtures do
  alias WorkReport.Model.{Day, Month, Task}

  def get_month_model_fixture do
    %Month{
      number: 1,
      title: "January",
      days: [
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
end
