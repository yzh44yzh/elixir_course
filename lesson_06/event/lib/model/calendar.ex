defmodule Model.Calendar do

  alias Model.CalendarItem

  @type t :: %__MODULE__{
    items: [CalendarItem]
  }
  @enforce_keys [:items]
  defstruct [:items]

  @spec new() :: t
  def new(), do: %__MODULE__{items: []}

  @spec add_item(t, CalendarItem.t) :: t
  def add_item(calendar, item) do
    %__MODULE__{calendar | items: [item | calendar.items]}
  end

  @spec show(t) :: String.t
  def show(calendar) do
    Enum.reduce(calendar.items, "",
      fn(item, acc) ->
        datetime = CalendarItem.datetime(item)
        description = CalendarItem.description(item)
        acc <> "#{datetime}: #{description}\n"
      end)
  end

end


defprotocol Model.CalendarItem do

  @spec datetime(any()) :: DateTime.t | nil
  def datetime(item)

  @spec description(any()) :: String.t | nil
  def description(item)

end


defimpl Model.CalendarItem, for: [Model.Event.Event, Model.TypedEvent.Event] do

  def datetime(event), do: event.datetime

  def description(event), do: event.title

end


defimpl Model.CalendarItem, for: Map do

  def datetime(event), do: event[:datetime]

  def description(event), do: event[:title]

end
