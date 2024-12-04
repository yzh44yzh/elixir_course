defmodule WorkReport.TerminalFormatter do
  alias WorkReport.{Formatter, Model.MonthReport}

  @behaviour Formatter

  @impl Formatter
  def format_report(month_report, opts) do
    # TODO
  end

  @spec format_time(integer) :: String.t()
  def format_time(_time) do
    "1h 30m"
  end
end
