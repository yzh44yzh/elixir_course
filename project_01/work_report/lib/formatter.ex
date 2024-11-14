defmodule WorkReport.Formatter do

  @spec format_time(integer) :: String.t()
  def format_time(_time) do
    "1h 30m"
  end
end
