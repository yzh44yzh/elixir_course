defmodule MarkdownParserTest do
  use ExUnit.Case

  alias WorkReport.{MarkdownParser, Model.Task}

  describe "parse_task" do
    test "should parse a task" do
      assert MarkdownParser.parse_task("[SOME] Do something useful - 30m") == %Task{
               category: "SOME",
               description: "Do something useful",
               time_spent: "30m"
             }
    end

    test "should return nil for invalid task" do
      assert MarkdownParser.parse_task("[SOME] Do something useful - eff") == nil
      assert MarkdownParser.parse_task("Some Do something useful - 30r") == nil
    end
  end

  describe "parse_time" do
    test "should parse time string" do
      assert MarkdownParser.parse_time("22m") == 22
      assert MarkdownParser.parse_time("42m") == 42
      assert MarkdownParser.parse_time("1m") == 1
      assert MarkdownParser.parse_time("0m") == 0
    end

    test "should parse time string with hours" do
      assert MarkdownParser.parse_time("1h 22m") == 82
    end
  end

  describe "string_to_int" do
    test "should convert string to int" do
      assert MarkdownParser.string_to_int("42") == 42
    end

    test "should return 0 for an empty string" do
      assert MarkdownParser.string_to_int("") == 0
    end
  end
end
