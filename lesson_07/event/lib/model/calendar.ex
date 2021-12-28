defmodule Model do

  defmodule Calendar do

    @type t :: %__MODULE__{
      items: [CalendarItem.t]
    }

    @enforce_keys [:items]
    defstruct [:items]

    @spec new() :: t
    def new() do
      %__MODULE__{items: []}
    end

    @spec add_item(t, CalendarItem.t) :: t
    def add_item(calendar, item) do
      items = [item | calendar.items]
      %__MODULE__{calendar | items: items}
    end

    @spec show(t) :: String.t
    def show(calendar) do
      Enum.map(calendar.items,
        fn(item) ->
          title = Model.CalendarItem.get_title(item)
          dt = Model.CalendarItem.get_datetime(item)
          "#{title} #{inspect dt}"
        end)
      |> Enum.join("/n")
    end

  end

  defprotocol CalendarItem do

    @spec get_title(CalendarItem.t) :: String.t
    def get_title(item)

    @spec get_datetime(CalendarItem.t) :: DateTime.t
    def get_datetime(item)

  end

  defimpl CalendarItem, for: Map do

    @spec get_title(CalendarItem.t) :: String.t
    def get_title(item) do
      Map.get(item, :title, "No Title")
    end

    @spec get_datetime(CalendarItem.t) :: DateTime.t
    def get_datetime(item) do
      Map.get(item, :datetime)
    end

  end

end
