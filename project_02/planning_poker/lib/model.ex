defmodule PlanningPoker.Model do
  @type role() :: :leader | :participant

  defmodule User do
    @type t() :: %__MODULE__{
            id: integer(),
            name: String.t(),
            role: Model.role()
          }

    defstruct [:id, :name, :role]
  end

  defmodule Room do
    @type t() :: %__MODULE__{
            name: String.t(),
            participants: [User.t()]
          }

    defstruct [:name, :participants]
  end
end
