defmodule WorkReport.Model do
  @moduledoc """
  Models' definitions
  """

  defmodule CategoryReport do
    @moduledoc false
    @type t() :: %__MODULE__{
            title: String.t(),
            time_spent: integer()
          }

    @enforce_keys [:title]

    defstruct [:title, time_spent: 0]
  end

  defmodule Task do
    @moduledoc false
    @type t() :: %__MODULE__{
            description: String.t(),
            time_spent: integer(),
            category: String.t()
          }

    @enforce_keys [:description, :time_spent, :category]

    defstruct [:description, :time_spent, :category]
  end

  defmodule Day do
    @moduledoc false

    @type t() :: %__MODULE__{
            number: integer(),
            tasks: [Task.t()],
            title: String.t()
          }

    @enforce_keys [:number, :title]

    defstruct [:number, :title, tasks: []]
  end

  defmodule Month do
    @moduledoc false
    @type t() :: %__MODULE__{
            days: [Day.t()],
            number: integer(),
            title: String.t()
          }

    @enforce_keys [:number, :title]

    defstruct [:number, :title, days: []]
  end

  defmodule DayReport do
    @moduledoc false
    @type t() :: %__MODULE__{
            tasks: [Task.t()],
            total_time_spent: integer()
          }

    @enforce_keys [:tasks, :total_time_spent]

    defstruct [:tasks, :total_time_spent]
  end

  defmodule MonthReport do
    @moduledoc false
    @type t() :: %__MODULE__{
            avg_time_spent: integer(),
            categories: [CategoryReport.t()],
            days_spent: integer(),
            total_time_spent: integer()
          }

    @enforce_keys [
      :avg_time_spent,
      :categories,
      :days_spent,
      :total_time_spent
    ]

    defstruct avg_time_spent: 0,
              categories: [],
              days_spent: 0,
              total_time_spent: 0
  end
end
