defmodule WorkReport do
  @moduledoc """
  # Analyze report file and gather work statistics
  """

  @name "Work Report"
  @version "1.0.0"

  def main(args) do
    case OptionParser.parse(args, options()) do
      {[help: true], [], []} -> help()
      {[version: true], [], []} -> version()
      {params, [file], []} -> parse(Map.new(params), file)
      _ -> help()
    end
  end

  defp options() do
    [
      strict: [day: :integer, month: :integer, version: :boolean, help: :boolean],
      aliases: [d: :day, m: :month, v: :version, h: :help]
    ]
  end

  defp parse(params, file) do
    month = Map.get(params, :month, :erlang.date() |> elem(1))
    day = Map.get(params, :day, :erlang.date() |> elem(2))

    file
    |> WorkReport.Parser.parse()
    |> show(month, day)
  end

  defp help() do
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

  defp version() do
    IO.puts(@name <> " v" <> @version)
  end

  alias WorkReport.Model, as: M
  alias WorkReport.Formatter

  @spec show([M.MonthReport.t()], integer, integer) :: :ok
  defp show(month_reports, month_num, day_num) do
    case Enum.find(month_reports, fn m -> m.month_num == month_num end) do
      nil ->
        IO.puts("month #{month_num} not found")

      month_report ->
        case Enum.find(month_report.days, fn d -> d.day_num == day_num end) do
          nil ->
            IO.puts("day #{day_num} not found")

          day_report ->
            Formatter.format_day(day_report) |> IO.puts()
            Formatter.format_month(month_report) |> IO.puts()
        end
    end
  end
end
