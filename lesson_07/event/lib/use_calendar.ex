defmodule CalendarExample do

  alias Model.Calendar, as: Cal

  def create() do
    cal = Cal.new()
    event_1 = StructExample.create()
    event_2 = TypedStructExample.create()
    event_3 = SimpleExample.create_map()
    # event_4 = SimpleExample.create()
    Cal.add_item(cal, event_1)
    |> Cal.add_item(event_2)
    |> Cal.add_item(event_3)
    # |> Cal.add_item(event_4)
  end

  def enum_test() do
    # dialyzer sees no error here:
    Enum.map(:not_an_enum, fn(i) -> i end)

    # dialyzer sees an error here:
    # Enum.map([1, 2, 3], :not_a_fun)
  end

end
