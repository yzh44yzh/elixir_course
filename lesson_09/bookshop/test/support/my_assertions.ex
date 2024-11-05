defmodule MyAssertions do
  use ExUnit.Case

  def assert_many(modules, fun, args, expected_result) do
    Enum.each(modules, fn module ->
      got_result = apply(module, fun, args)
      assert got_result == expected_result
    end)
  end
end
