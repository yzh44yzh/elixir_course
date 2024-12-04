defmodule MarkdownParserTest do
  use ExUnit.Case

  alias WorkReport.{MarkdownParser}
  alias WorkReport.Model.{Day, Task}

  # TODO: Add StreamData package to tests for property-based testing

  describe "parse_task" do
    test "should parse a task" do
      assert MarkdownParser.parse_task("[SOME] Do something useful - 30m") == %Task{
               category: "SOME",
               description: "Do something useful",
               time_spent: 30
             }
    end

    test "should return nil for invalid task" do
      assert MarkdownParser.parse_task("[SOME] Do something useful - eff") == nil
      assert MarkdownParser.parse_task("Some Do something useful - 30r") == nil
    end
  end

  describe "parse_day_string" do
    test "should parse a day string to map" do
      assert MarkdownParser.parse_day_string("## 3 mon") == %Day{
               number: 3,
               title: "mon",
               tasks: []
             }
    end
  end

  describe "parse_task_list" do
    test "should parse task string list" do
      assert MarkdownParser.parse_task_list(%Day{number: 1, title: "mon"}, [
               "[SOME] Test title - 15m",
               "[DEV] Do some useful stuff - 2h",
               "[COM] Daily meeting with indians - 5h 30m"
             ]) == %Day{
               number: 1,
               title: "mon",
               tasks: [
                 %Task{category: "SOME", description: "Test title", time_spent: 15},
                 %Task{category: "DEV", description: "Do some useful stuff", time_spent: 120},
                 %Task{
                   category: "COM",
                   description: "Daily meeting with indians",
                   time_spent: 330
                 }
               ]
             }
    end
  end

  # describe "parse_day" do
  test "should" do
    assert MarkdownParser.parse_day(
             "## 3 mon\n[DEV] Review Shitty Pull Requests - 30m\n[COMM] Daily Meeting with arabs - 15m\n[DEV] Implement cool feature for legacy monster - 4h\n"
           ) == %Day{
             number: 3,
             title: "mon",
             tasks: [
               %Task{category: "DEV", description: "Review Shitty Pull Requests", time_spent: 30},
               %Task{category: "COMM", description: "Daily Meeting with arabs", time_spent: 15},
               %Task{
                 category: "DEV",
                 description: "Implement cool feature for legacy monster",
                 time_spent: 240
               }
             ]
           }
  end

  # end

  describe "parse_time" do
    test "should parse time string" do
      assert MarkdownParser.parse_time("1h 22m") == 82
      assert MarkdownParser.parse_time("2h 2m") == 122
    end

    test "should parse time string minutes only" do
      assert MarkdownParser.parse_time("22m") == 22
      assert MarkdownParser.parse_time("42m") == 42
      assert MarkdownParser.parse_time("1m") == 1
      assert MarkdownParser.parse_time("0m") == 0
    end

    test "should parse time string hours only" do
      assert MarkdownParser.parse_time("1h") == 60
      assert MarkdownParser.parse_time("2h") == 120
      assert MarkdownParser.parse_time("15h") == 900
    end
  end

  describe "string_to_int" do
    test "should convert string to int" do
      assert MarkdownParser.string_to_int("42") == 42
    end

    test "should convert string to int with trailing zero" do
      assert MarkdownParser.string_to_int("02") == 2
    end

    test "should return 0 for an empty string" do
      assert MarkdownParser.string_to_int("") == 0
    end
  end
end
