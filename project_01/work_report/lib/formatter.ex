defmodule WorkReport.Formatter do
  alias WorkReport.Model.MonthReport

  @callback format_report(month_report :: MonthReport.t(), opts :: Keyword.t()) :: binary()

  @spec print_report(month_report :: MonthReport.t(), opts :: Keyword.t()) :: binary()
  def print_report(month_report, opts) do
    {formatter, opts} = Keyword.pop(opts, :formatter)

    formatter.format_report(month_report, opts)
  end

  @spec format_time(integer) :: String.t()
  def format_time(_time) do
    "1h 30m"
  end
end
