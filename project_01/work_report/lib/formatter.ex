defmodule WorkReport.Formatter do
  @moduledoc """
  Interface for report formatter.
  """
  alias WorkReport.Model.Report

  @callback format_report_list(report_list :: [Report.t()], opts :: Keyword.t()) ::
              binary()

  @spec print_report(report_list :: [Report.t()], opts :: Keyword.t()) ::
          binary()
  def print_report(report_list, opts) do
    {formatter, opts} = Keyword.pop(opts, :formatter)

    formatter.format_report_list(report_list, opts)
  end
end
