defmodule Lesson_03.Task_03_03_Bool do
  @moduledoc """
  Таблицы истинности для троичной логики Стивена Клини
  https://ru.wikipedia.org/wiki/%D0%A2%D1%80%D0%BE%D0%B8%D1%87%D0%BD%D0%B0%D1%8F_%D0%BB%D0%BE%D0%B3%D0%B8%D0%BA%D0%B0

  Truth tables for Stephen Kleene's "strong logic of indeterminacy"
  https://en.wikipedia.org/wiki/Three-valued_logic
  """

  defmodule Step1 do
    
    def sk_not(false), do: true
    def sk_not(nil), do: nil
    def sk_not(true), do: false
    
    def sk_and(false, false), do: false
    def sk_and(false, nil), do: false
    def sk_and(false, true), do: false
    def sk_and(nil, false), do: false
    def sk_and(nil, nil), do: nil
    def sk_and(nil, true), do: nil
    def sk_and(true, false), do: false
    def sk_and(true, nil), do: nil
    def sk_and(true, true), do: true

    def sk_or(false, false), do: false
    def sk_or(false, nil), do: nil
    def sk_or(false, true), do: true
    def sk_or(nil, false), do: nil
    def sk_or(nil, nil), do: nil
    def sk_or(nil, true), do: true
    def sk_or(true, false), do: true
    def sk_or(true, nil), do: true
    def sk_or(true, true), do: true
    
  end

  defmodule Step2 do
    
    def sk_not(nil), do: nil
    def sk_not(arg), do: not arg
    
    def sk_and(false, _), do: false
    def sk_and(nil, false), do: false
    def sk_and(nil, _), do: nil
    def sk_and(true, second_arg), do: second_arg

    def sk_or(true, _), do: true
    def sk_or(nil, true), do: true
    def sk_or(nil, _), do: nil
    def sk_or(false, second_arg), do: second_arg
    
  end

end

ExUnit.start()

defmodule Task_03_03_Test do
  use ExUnit.Case
  # import Lesson_03.Task_03_03_Bool.Step1
  import Lesson_03.Task_03_03_Bool.Step2

  test "Stephen Kleene, not" do
    assert sk_not(true) == false
    assert sk_not(nil) == nil
    assert sk_not(false) == true
  end

  test "Stephen Kleene, and" do
    assert sk_and(false, false) == false
    assert sk_and(false, nil) == false
    assert sk_and(false, true) == false
    assert sk_and(nil, false) == false
    assert sk_and(nil, nil) == nil
    assert sk_and(nil, true) == nil
    assert sk_and(true, false) == false
    assert sk_and(true, nil) == nil
    assert sk_and(true, true) == true
  end

  test "Stephen Kleene, or" do
    assert sk_or(false, false) == false
    assert sk_or(false, nil) == nil
    assert sk_or(false, true) == true
    assert sk_or(nil, false) == nil
    assert sk_or(nil, nil) == nil
    assert sk_or(nil, true) == true
    assert sk_or(true, false) == true
    assert sk_or(true, nil) == true
    assert sk_or(true, true) == true
  end

end
