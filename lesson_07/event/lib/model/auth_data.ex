defmodule Model.AuthData do

  @type t :: %__MODULE__{
    login: String.t,
    password: String.t
  }

  @enforce_keys [:login, :password]

  defstruct [:login, :password]

end


defmodule Model.AuthDataS do

  @type t :: %__MODULE__{
    login: String.t,
    password: String.t
  }

  @enforce_keys [:login, :password]

  @derive {Inspect, except: [:password]}
  defstruct [:login, :password]

end


defmodule Model.AuthDataC do

  @type t :: %__MODULE__{
    login: String.t,
    password: String.t
  }

  @enforce_keys [:login, :password]

  defstruct [:login, :password]

  defimpl Inspect do
    alias Inspect.Algebra, as: A

    def inspect(auth_data, opts) do
      A.concat(["#AuthData<login:", A.to_doc(auth_data.login, opts), ">"])
    end
  end

end
