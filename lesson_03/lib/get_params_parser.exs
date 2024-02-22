defmodule Parser do

  def test_str(), do: "foo=42&bar=100&bazz=500"

  def parse(str) do
    str
    |> String.split("&")
    |> parse(%{})
  end

  def parse([], acc), do: acc

  def parse([pair | rest], acc) do
    case String.split(pair, "=") do
      [key, value] ->
        parse(rest, Map.put(acc, key, value))
      _ ->
        parse(rest, acc)
    end
  end

end

ExUnit.start()

defmodule ParserTest do
  use ExUnit.Case
  import Parser

  test "parse" do
    assert parse("a=1&b=2") == %{"a" => "1", "b" => "2"}
    assert parse("a=1&b=2&c=3&a=4") == %{"a" => "4", "b" => "2", "c" => "3"}
    assert parse("foo=42&bar=100&bazz=500") == %{"foo" => "42", "bar" => "100", "bazz" => "500"}
  end

  test "parse corner cases" do
    assert parse("a=1&b=2&dd") == %{"a" => "1", "b" => "2"}
    assert parse("a=1&b=2&dd=ff=gg") == %{"a" => "1", "b" => "2"}
  end

end
