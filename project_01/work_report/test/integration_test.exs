defmodule IntegrationTest do
  use ExUnit.Case

  setup do
    {_, 0} = System.cmd("mix", ["escript.build"])
    :ok
  end

  @tag :integration
  test "report-1" do
    report_1 = get_report_path("report-1")
    call_and_check_response(["--month=5", "--day=3", report_1], "response-1-1")
    call_and_check_response(["--month=5", "--day=4", report_1], "response-1-2")
    call_and_check_response(["--month=5", "--day=5", report_1], "response-1-3")
  end

  @tag :integration
  test "report-2" do
    report_2 = get_report_path("report-2")
    call_and_check_response(["--month=3", "--day=9", report_2], "response-2-1")
    call_and_check_response(["--month=3", "--day=10", report_2], "response-2-2")
    call_and_check_response(["--month=4", "--day=15", report_2], "response-2-3")
    call_and_check_response(["--month=4", "--day=16", report_2], "response-2-4")
  end

  @tag :integration
  test "invalid month or day" do
    report_1 = get_report_path("report-1")
    call_and_check_response(["--month=1", "--day=1", report_1], "response-month-not-found")
    call_and_check_response(["--month=5", "--day=1", report_1], "response-day-not-found")
  end

  test "help and version" do
    call_and_check_response(["--help"], "response-help")
    call_and_check_response(["--version"], "response-version")
  end

  defp get_report_path(report_name) do
    File.cwd!() |> Path.join("test/sample/#{report_name}.md")
  end

  defp call_and_check_response(args, response_name) do
    {:ok, curr_dir} = File.cwd()
    work_report_script = Path.join(curr_dir, "work_report")

    {response, exit_status} = System.cmd(work_report_script, args)
    assert exit_status == 0

    {:ok, expected_response} =
      Path.join(curr_dir, "test/sample/#{response_name}.md") |> File.read()

    assert response == expected_response
  end
end
