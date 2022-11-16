defmodule PlanningPoker.Model do

  @type role() :: :manager | :participant

  defmodule Vote do
    @type t() :: %__MODULE__{
      user_name: String.t(),
      points: pos_integer()
    }

    @enforce_keys [:user_name, :points]
    defstruct [:user_name, :points]
  end

  defmodule User do
    @type t() :: %__MODULE__{
      user_name: String.t(),
      role: Model.role()
    }

    @enforce_keys [:user_name, :role]
    defstruct [:user_name, :role]
  end

  defmodule Session do
    @type t() :: %__MODULE__{
      handler: pid(),
      user: User.t()
    }

    @enforce_keys [:handler, :user]
    defstruct [:handler, :user]
  end

end
