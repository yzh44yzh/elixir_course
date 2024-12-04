defmodule WorkReport.Parser do
  @callback parse_report(report_file, parser) :: {:ok, any()}

  @spec parse_time(String.t()) :: integer()
  def parse_time(_time_str) do
    42
  end
end
