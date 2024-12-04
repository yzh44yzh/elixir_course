defmodule WorkReport.MarkdownParser do
  alias WorkReport.Model.{Day, Month, Task}
  alias WorkReport.Parser

  @behaviour Parser

  @minutes_in_one_hour 60

  defmodule InvalidMonthTitleError do
    defexception [:message]

    @impl true
    def exception(month_title) do
      %InvalidMonthTitleError{message: "Wrong month name given! Got: \"#{month_title}\""}
    end
  end

  defmodule InvalidDayStringError do
    defexception [:message]

    @impl true
    def exception(day_string) do
      %InvalidDayStringError{message: "Invalid day string: #{day_string}"}
    end
  end

  @impl Parser
  def parse_report(report, opts) do
    [month_string | day_string_list] =
      report
      |> String.split("\n\n")

    month = parse_month_string(month_string)
    days = day_string_list |> Enum.map(&parse_day/1)

    Map.put(month, :days, days)
  end

  @spec get_month_number(month_title :: String.t()) :: integer() | nil
  def get_month_number(month_title) do
    case month_title do
      "January" -> 1
      "February" -> 2
      "March" -> 3
      "April" -> 4
      "May" -> 5
      "June" -> 6
      "July" -> 7
      "August" -> 8
      "September" -> 9
      "October" -> 10
      "November" -> 11
      "December" -> 12
      _ -> raise InvalidMonthTitleError, month_title
    end
  end

  @spec parse_month_string(full_month_string :: String.t()) :: Month.t()
  def parse_month_string(full_month_string) do
    regexp = ~r/^#\s(?<title>\w+)$/

    case Regex.named_captures(regexp, full_month_string) do
      %{"title" => title} ->
        %Month{number: get_month_number(title), title: title}

      nil ->
        nil
    end
  end

  @spec parse_day(full_day_string :: binary()) :: Day.t()
  def parse_day(full_day_string) do
    [day_string | task_string_list] = String.split(full_day_string, "\n")

    parse_day_string(day_string) |> parse_task_list(task_string_list)
  end

  @spec parse_day_string(day_string :: binary()) :: Day.t()
  def parse_day_string(day_string) do
    regexp = ~r/^##\s(?<number>\d+)\s(?<title>\w+)$/

    case Regex.named_captures(regexp, day_string) do
      %{"number" => number, "title" => title} ->
        %Day{number: String.to_integer(number), title: title}

      nil ->
        raise InvalidDayStringError, day_string
    end
  end

  @spec parse_task_list(day :: Day.t(), task_string_list :: [binary()]) :: Day.t()
  def parse_task_list(day, task_string_list) do
    tasks = Enum.map(task_string_list, &parse_task/1) |> Enum.reject(&is_nil/1)

    Map.put(day, :tasks, tasks)
  end

  @spec parse_task(task :: binary()) :: Task.t()
  def parse_task(task) do
    # Regexp to parse task string format: [Type] Some title - 1h 30m
    regexp =
      ~r/^\[(?<category>\w+)\]\s(?<description>.+)\s\-\s(?<time_spent>(\d{0,2}h)?\s?(\d{0,2}m)?)$/

    case Regex.named_captures(regexp, task) do
      nil ->
        nil

      %{"category" => category, "description" => description, "time_spent" => time_spent} ->
        %Task{category: category, description: description, time_spent: parse_time(time_spent)}
    end
  end

  @spec parse_time(String.t()) :: integer()
  def parse_time(time_string) do
    %{"hours" => hours, "minutes" => minutes} =
      Regex.named_captures(~r/^((?<hours>\d{0,2})h)?\s?((?<minutes>\d{0,2})m)?$/, time_string)

    string_to_int(hours) * @minutes_in_one_hour + string_to_int(minutes)
  end

  def string_to_int(""), do: 0
  def string_to_int(str), do: String.to_integer(str)
end
