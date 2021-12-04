defmodule ChatRoomModel do

  @type user_name :: String.t
  @type room_name :: String.t
  @type room_type :: :public | :private

  defmodule User do
    alias ChatRoomModel, as: M
    
    @type t :: %__MODULE__{
      name: M.user_name
    }
    @enforce_keys [:name]
    defstruct [
      :name
    ]
  end
  
  defmodule Room do
    alias ChatRoomModel, as: M

    @type t :: %__MODULE__{
      name: M.room_name,
      type: M.room_type,
      members: [M.user_name],
      limit: pos_integer
    }
    @enforce_keys [:name]
    defstruct [
      :name,
      {:type, :public},
      {:members, []},
      {:limit, 100}
    ]
  end

end
