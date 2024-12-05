defmodule WorkReport do
  @moduledoc """
  # Analyze report file and gather work statistics
  """

  alias WorkReport.{
    Parser,
    MarkdownParser,
    ReportBuilder,
    ReportBuilder.MonthNotFoundError,
    ReportBuilder.DayNotFoundError,
    Formatter,
    TerminalFormatter
  }

  @name "Work Report"
  @version "1.0.0"

  @spec main([String.t()]) :: :ok
  def main(args) do
    case OptionParser.parse(args, options()) do
      {[help: true], [], []} -> help()
      {[version: true], [], []} -> version()
      {params, [file], []} -> do_report(Map.new(params), file)
      _ -> help()
    end
  end

  def options do
    [
      strict: [day: :integer, month: :integer, version: :boolean, help: :boolean],
      aliases: [d: :day, m: :month, v: :version, h: :help]
    ]
  end

  def do_report(params, file) do
    month = Map.get(params, :month, :erlang.date() |> elem(1))
    day = Map.get(params, :day, :erlang.date() |> elem(2))

    cond do
      month not in 1..12 -> raise "Month is out of range 1..12"
      day not in 1..31 -> raise "Day is out of range 1..31"
      true -> build_report(file, month: month, day: day)
    end
  end

  def build_report(path, opts) do
    {month, opts} = Keyword.pop(opts, :month)
    {day, _opts} = Keyword.pop(opts, :day)

    result =
      try do
        with {:ok, report_model} <- Parser.build_month_model(path, parser: MarkdownParser) do
          report_model
          |> ReportBuilder.build_report(month, day)
          |> Formatter.print_report(formatter: TerminalFormatter)
        end
      rescue
        e in [MonthNotFoundError, DayNotFoundError] -> e.message
      end

    IO.puts(result)
  end

  def help() do
    IO.puts("""
    USAGE:
        work_report [OPTIONS] <path/to/report.md>
    OPTIONS:
        -m, --month <M>  Show report for month (int), current month by default
        -d, --day <D>    Show report for day (int), current day by default
        -v, --version    Show version
        -h, --help       Show this help message
    """)
  end

  def version() do
    IO.puts(@name <> " v" <> @version)
  end
end
