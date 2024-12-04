defmodule WorkReport.MarkdowParser do
  alias WorkReport.Model.Task
  @behaviour WorkReport.Parser

  @impl WorkReport.Parser
  def parse_report(report, opts) do
    [month | days] =
      report
      |> String.split("\n\n")

    days
    |> Enum.map(fn day -> String.split(day, "\n") end)
    |> Enum.map(fn [day | tasks] ->
      [
        day |> String.split(" "),
        tasks
        |> Enum.map(&parse_task/1)
        |> Enum.reject(&is_nil/1)
      ]
    end)
  end

  @spec parse_task(task :: binary()) :: Task.t()
  defp parse_task(task) do
    task |> dbg()
    regexp = ~r/^(\[\w+\])\s(.+)\s\-\s([\w+|\s]+)$/
    result = Regex.scan(regexp, task)

    case result do
      [] ->
        nil

      [[_, category, description, time_spent]] ->
        # TODO: convert time to minutes
        %Task{category: category, description: description, time_spent: time_spent}
    end
  end
end
