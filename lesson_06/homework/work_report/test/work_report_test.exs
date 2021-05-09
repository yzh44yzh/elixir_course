defmodule WorkReportTest do
  use ExUnit.Case

  test "parse time" do
    assert WrParser.parse_time("1m") == 1
    assert WrParser.parse_time("5m") == 5
    assert WrParser.parse_time("12m") == 12
    assert WrParser.parse_time("42m") == 42
    assert WrParser.parse_time("59m") == 59
    assert WrParser.parse_time("60m") == 60
    assert WrParser.parse_time("61m") == 61
    assert WrParser.parse_time("1h") == 60
    assert WrParser.parse_time("1h 5m") == 65
    assert WrParser.parse_time("1h 30m") == 90
    assert WrParser.parse_time("2h 20m") == 140
    assert WrParser.parse_time("1h 90m") == 150
    assert WrParser.parse_time("3h") == 180
    assert WrParser.parse_time("10h") == 600
    assert WrParser.parse_time("10h 15m") == 615

    assert WrParser.parse_time("") == 0
    assert WrParser.parse_time("0m") == 0
    assert WrParser.parse_time("0h") == 0
    assert WrParser.parse_time("0m 0h") == 0
    assert WrParser.parse_time("whatever") == 0
  end

  test "format time" do
    assert WrFormatter.format_time(0) == "0"
    assert WrFormatter.format_time(1) == "1m"
    assert WrFormatter.format_time(12) == "12m"
    assert WrFormatter.format_time(42) == "42m"
    assert WrFormatter.format_time(59) == "59m"
    assert WrFormatter.format_time(60) == "1h"
    assert WrFormatter.format_time(61) == "1h 1m"
    assert WrFormatter.format_time(65) == "1h 5m"
    assert WrFormatter.format_time(90) == "1h 30m"
    assert WrFormatter.format_time(140) == "2h 20m"
    assert WrFormatter.format_time(150) == "2h 30m"
    assert WrFormatter.format_time(180) == "3h"
    assert WrFormatter.format_time(600) == "10h"
    assert WrFormatter.format_time(615) == "10h 15m"
  end

end
