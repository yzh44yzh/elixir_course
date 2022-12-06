defmodule WorkReport.Model do
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

    def new(day), do: %__MODULE__{day: day, day_num: get_day_num(day)}

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

    def new(month), do: %__MODULE__{month: month, month_num: get_month_num(month)}

    def add_day(report, day) do
      %__MODULE__{report | days: [day | report.days]}
    end

    defp get_month_num("January"), do: 1
    defp get_month_num("February"), do: 2
    defp get_month_num("March"), do: 3
    defp get_month_num("April"), do: 4
    defp get_month_num("May"), do: 5
    defp get_month_num("June"), do: 6
    defp get_month_num("July"), do: 7
    defp get_month_num("August"), do: 8
    defp get_month_num("September"), do: 9
    defp get_month_num("October"), do: 10
    defp get_month_num("November"), do: 11
    defp get_month_num("December"), do: 12

    defp get_month_num(month) do
      raise("unknown month #{inspect(month)}")
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
