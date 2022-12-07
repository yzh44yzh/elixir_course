defmodule WorkReport.Model do
  def monthes do
    [
      {1, "January"},
      {2, "February"},
      {3, "March"},
      {4, "April"},
      {5, "May"},
      {6, "June"},
      {7, "July"},
      {8, "August"},
      {9, "September"},
      {10, "October"},
      {11, "November"},
      {12, "December"}
    ]
  end

  def categories do
    ["COMM", "DEV", "OPS", "DOC", "WS", "EDU"]
  end

  defmodule Task do
    @type t :: %__MODULE__{
            category: String.t(),
            description: String.t(),
            time: integer
          }
    @enforce_keys [:category, :description, :time]
    defstruct [:category, :description, :time]

    def new(category, description, time) do
      %__MODULE__{category: category, description: description, time: time}
    end
  end

  defmodule DayReport do
    @type t :: %__MODULE__{
            day_num: integer,
            day: String.t(),
            tasks: [Task.t()]
          }
    @enforce_keys [:day]
    defstruct day_num: 0, day: nil, tasks: []

    def new(day, tasks \\ []) do
      %__MODULE__{
        day: day,
        day_num: get_day_num(day),
        tasks: tasks
      }
    end

    def add_task(report, task) do
      %__MODULE__{report | tasks: [task | report.tasks]}
    end

    defp get_day_num(day) do
      case Integer.parse(day) do
        {num, _} when num >= 1 and num <= 31 -> num
        _ -> raise "unknown day #{inspect(day)}"
      end
    end
  end

  defmodule MonthReport do
    @type t :: %__MODULE__{
            month_num: integer,
            month: String.t(),
            days: [DayReport.t()]
          }
    @enforce_keys [:month]
    defstruct month_num: 0, month: nil, days: []

    def new(month, days \\ []) do
      %__MODULE__{
        month: month,
        month_num: get_month_num(month),
        days: days
      }
    end

    def add_day(report, day) do
      %__MODULE__{report | days: [day | report.days]}
    end

    defp get_month_num(month) do
      WorkReport.Model.monthes()
      |> List.keyfind(month, 1, :not_found)
      |> case do
        {num, ^month} -> num
        :not_found -> raise("unknown month #{inspect(month)}")
      end
    end
  end

  defmodule TotalReport do
    @type t :: %__MODULE__{
            months: [MonthReport.t()]
          }
    @enforce_keys []
    defstruct months: []

    def new(), do: %__MODULE__{}

    def add_month(report, month) do
      %__MODULE__{report | months: [month | report.months]}
    end
  end
end
