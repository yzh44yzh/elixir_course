defmodule MyCalendar.Model do
  defmodule Calendar do
    alias MyCalendar.Model.CalendarItem

    @type t :: %__MODULE__{
            items: [CalendarItem.t()]
          }

    @enforce_keys [:items]
    defstruct [:items]

    @spec add_item(Calendar.t(), CalendarItem.t()) :: Calendar.t()
    def add_item(calendar, item) do
      items = [item | calendar.items]
      %__MODULE__{calendar | items: items}
    end

    @spec show(Calendar.t()) :: String.t()
    def show(calendar) do
      Enum.map(
        calendar.items,
        fn item ->
          title = CalendarItem.get_title(item)
          time = CalendarItem.get_time(item) |> DateTime.to_iso8601()
          "#{title} at #{time}"
        end
      )
      |> Enum.join("\n")
    end
  end

  defprotocol CalendarItem do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title(item)

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time(item)
  end

  defimpl CalendarItem, for: Map do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title(item), do: Map.get(item, :title, "No Title")

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time(item), do: Map.get(item, :time)
  end

  defimpl CalendarItem, for: Tuple do
    @spec get_title(CalendarItem.t()) :: String.t()
    def get_title({:event, title, _, _, _, _}), do: title

    @spec get_time(CalendarItem.t()) :: DateTime.t()
    def get_time({:event, _, _, time, _, _}), do: time
  end

  # defimpl CalendarItem, for: MyCalendar.Model.Struct.Event do
  #   @spec get_title(CalendarItem.t()) :: String.t()
  #   def get_title(event), do: event.title

  #   @spec get_time(CalendarItem.t()) :: DateTime.t()
  #   def get_time(event), do: event.time
  # end

  # defimpl CalendarItem, for: MyCalendar.Model.TypedStruct.Event do
  #   @spec get_title(CalendarItem.t()) :: String.t()
  #   def get_title(event), do: event.title

  #   @spec get_time(CalendarItem.t()) :: DateTime.t()
  #   def get_time(event), do: event.time
  # end
end
