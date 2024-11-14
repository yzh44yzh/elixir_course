defmodule ParserTest do
  use ExUnit.Case

  alias WorkReport.Parser

  test "parse time" do
    assert Parser.parse_time("1m") == 1
    assert Parser.parse_time("5m") == 5
    assert Parser.parse_time("12m") == 12
    assert Parser.parse_time("42m") == 42
    assert Parser.parse_time("59m") == 59
    assert Parser.parse_time("60m") == 60
    assert Parser.parse_time("61m") == 61
    assert Parser.parse_time("1h") == 60
    assert Parser.parse_time("1h 5m") == 65
    assert Parser.parse_time("1h 30m") == 90
    assert Parser.parse_time("2h 20m") == 140
    assert Parser.parse_time("1h 90m") == 150
    assert Parser.parse_time("3h") == 180
    assert Parser.parse_time("10h") == 600
    assert Parser.parse_time("10h 15m") == 615

    assert Parser.parse_time("") == 0
    assert Parser.parse_time("0m") == 0
    assert Parser.parse_time("0h") == 0
    assert Parser.parse_time("0m 0h") == 0
    assert Parser.parse_time("whatever") == 0
  end
end
