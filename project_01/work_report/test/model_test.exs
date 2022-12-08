defmodule ModelTest do
  use ExUnit.Case

  alias WorkReport.Model

  test "get day num" do
    assert Model.DayReport.get_day_num("1 mon") == 1
    assert Model.DayReport.get_day_num("10 wed") == 10
    assert Model.DayReport.get_day_num("31 fri") == 31

    assert_raise RuntimeError, "unknown day \"0 mon\"", fn ->
      Model.DayReport.get_day_num("0 mon")
    end

    assert_raise RuntimeError, "unknown day \"32 mon\"", fn ->
      Model.DayReport.get_day_num("32 mon")
    end

    assert_raise RuntimeError, "unknown day \"mon\"", fn ->
      Model.DayReport.get_day_num("mon")
    end
  end
end
