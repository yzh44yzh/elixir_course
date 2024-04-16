defmodule DoEnd do

  def my_fun(arg1, options) do
    IO.puts("my_fun called with arg1:#{arg1} and options:#{inspect options}")
  end

  def if_1(condition) do
    if condition do
      a = 42
      {:true_branch, a + a}
    else
      b = 50
      {:false_branch, b * b}
    end
  end

  def if_2(condition) do
    if condition, do: (
      a = 42
      {:true_branch, a + a}
    ),
    else: (
      b = 50
      {:false_branch, b * 2}
    )
  end

  def if_3(condition) do
    if(condition, [
      do: (
        a = 42; {:true_branch, a + a}
      ),
      else: (
        b = 50; {:false_branch, b * b}
      )
    ])
  end

  def if_4(condition) do
    code_block_1 = (a = 42; {:true_branch, a + a})
    code_block_2 = (b = 50; {:false_branch, b * b})
    if(condition, [{:do, code_block_1}, {:else, code_block_2}])
  end

  def some_fun_1(arg1, arg2) do
    a = 42
    arg1 + arg2 + a
  end

  def some_fun_2(arg1, arg2), do: (a = 42; arg1 + arg2 + a)

  def( some_fun_3(arg1, arg2), [{:do, (a = 42; arg1 + arg2 + a)}] )

  defmodule(MyModule, [{:do, (def f1(), do: 42; def f2(), do: 100)}])

end
