defmodule WorkReport do
  @moduledoc """
  # Analyze report file and gather work statistics
  """

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

  def do_report(params, _file) do
    _month = Map.get(params, :month, :erlang.date() |> elem(1))
    _day = Map.get(params, :day, :erlang.date() |> elem(2))

    # TODO your implementation here
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
