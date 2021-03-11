ExUnit.start()

defmodule QuadraticEquationTest do
  use ExUnit.Case
  doctest QuadraticEquation
  import QuadraticEquation

  test "solve" do
    assert solve(1, -2, -3) == {:roots, 3, -1}
    assert solve(-1, -2, 15) == {:roots, -5, 3}
    assert solve(1, 12, 36) == {:root, -6}
    assert solve(5, 3, 7) == :no_roots
  end

  test "solve reduced" do
    assert solve(4, 0, -9) == {:roots, 1.5, -1.5}
    assert solve(1, -7, 0) == {:roots, 7, 0}
    assert solve(42, 0, 0) == {:root, 0}
    assert solve(5, 0, 30) == :no_roots
  end

end