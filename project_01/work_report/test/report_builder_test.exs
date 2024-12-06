defmodule ReportBuilderTest do
  use ExUnit.Case

  alias WorkReport.ReportBuilder
  alias WorkReport.ReportBuilder.{DayNotFoundError, MonthNotFoundError}

  import TestFixtures

  describe "build_report" do
    test "should build single month report" do
      assert ReportBuilder.build_report(single_model_list_fixture_2(), 1, 3) ==
               single_model_list_report_fixture_2_m1_d3()
    end

    test "should build plural month report" do
      model = plural_model_list_fixture_1()

      assert ReportBuilder.build_report(model, 6, 5) ==
               plural_model_list_report_fixture_1_m6_d5()

      assert ReportBuilder.build_report(model, 6, 5) ==
               plural_model_list_report_fixture_1_m6_d5()
    end

    test "should raise an error for wrong month number" do
      assert_raise MonthNotFoundError, "month 2 not found", fn ->
        ReportBuilder.build_report(single_model_list_fixture_2(), 2, 3)
      end
    end

    test "should raise an error for wrong day number" do
      assert_raise DayNotFoundError, "day 22 not found", fn ->
        ReportBuilder.build_report(single_model_list_fixture_2(), 1, 22)
      end
    end
  end
end
