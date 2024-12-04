defmodule WorkReport.Parser do
  alias Model.Month

  @callback parse_report(report :: binary(), opts :: Keyword.t()) ::
              {:ok, Month.t()} | {:error, String.t()}

  @spec build_month_model(report_file_path :: binary(), opts :: Keyword.t()) :: {:ok, Month.t()}
  def build_month_model(report_file_path, opts) do
    {parser, opts} = Keyword.pop(opts, :parser)
    file = File.read!(report_file_path)

    parser.parse_report(file, opts)
  end

  @spec parse_time(String.t()) :: integer()
  def parse_time(_time_str) do
    42
  end
end
