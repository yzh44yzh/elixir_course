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

  defp parse(params, report_file) do
    month = Map.get(params, :month, :erlang.date() |> elem(1))
    day = Map.get(params, :day, :erlang.date() |> elem(2))
    show(report_file, month, day)
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

  defp show(_report_file, _month_num, _day_num) do
    IO.puts("not implemented")
  end
end
