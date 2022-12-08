defmodule FormatterTest do
  use ExUnit.Case

  alias WorkReport.Formatter

  test "format time" do
    assert Formatter.format_time(0) == "0"
    assert Formatter.format_time(1) == "1m"
    assert Formatter.format_time(12) == "12m"
    assert Formatter.format_time(42) == "42m"
    assert Formatter.format_time(59) == "59m"
    assert Formatter.format_time(60) == "1h"
    assert Formatter.format_time(61) == "1h 1m"
    assert Formatter.format_time(65) == "1h 5m"
    assert Formatter.format_time(90) == "1h 30m"
    assert Formatter.format_time(140) == "2h 20m"
    assert Formatter.format_time(150) == "2h 30m"
    assert Formatter.format_time(180) == "3h"
    assert Formatter.format_time(600) == "10h"
    assert Formatter.format_time(615) == "10h 15m"
  end
end
