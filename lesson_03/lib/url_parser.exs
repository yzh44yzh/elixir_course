defmodule UrlParser do

  def test_url() do
    "https://elixir-lang.org/section/subsection/page.html?a=42&b=100&c=500"
  end

  def parse(url) do
    [protocol, rest] = String.split(url, "://")
    [domain, rest] = String.split(rest, "/", parts: 2)
    [path, rest] = String.split(rest, "?")
    %{
      protocol: protocol,
      domain: domain,
      path: path,
      params: parse_params(rest)
    }
  end

  def parse_params(str) do
    String.split(str, "&")
    |> parse_params(%{})
  end

  defp parse_params([], acc), do: acc

  defp parse_params([pair | pairs], acc) do
    case String.split(pair, "=") do
      [key, value] ->
        acc = Map.put(acc, key, value)
        parse_params(pairs, acc)
      _ ->
        parse_params(pairs, acc)
    end
  end
end

ExUnit.start()

defmodule UrlParserTest do
  use ExUnit.Case
  import UrlParser

  test "parse_params" do
    assert parse_params("") == %{}
    assert parse_params("a=42") == %{"a" => "42"}
    assert parse_params("a=42&b=100&c=500&d=hello") == %{
             "a" => "42",
             "b" => "100",
             "c" => "500",
             "d" => "hello"
           }
  end

  test "parse_params with invalid kv" do
    assert parse_params("a=42&b100&c=500&d=hello") == %{
             "a" => "42",
             "c" => "500",
             "d" => "hello"
           }
  end

  test "parse" do
    assert parse(test_url()) == %{
             protocol: "https",
             domain: "elixir-lang.org",
             path: "section/subsection/page.html",
             params: %{
               "a" => "42",
               "b" => "100",
               "c" => "500"
             }
           }
  end
end
