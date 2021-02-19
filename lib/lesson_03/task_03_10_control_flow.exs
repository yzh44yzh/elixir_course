defmodule Lesson_03.Task_03_10_ControlFlow do

  def handle(animal, action) do
    case animal do
      {:dog, name} ->
        case action do
          :add -> IO.puts("add dog #{name}")
          :remove -> IO.puts("remove dog #{name}")
        end
      {:cat, name} ->
        case action do
          :add -> IO.puts("add cat #{name}")
          :remove -> IO.puts("remove cat #{name}")
        end
    end
  end

  def handle2(animal, action) do
    case {animal, action} do
      {{:dog, name}, :add} -> IO.puts("add dog #{name}")
      {{:dog, name}, :remove} -> IO.puts("remove dog #{name}")
      {{:cat, name}, :add} -> IO.puts("add cat #{name}")
      {{:cat, name}, :remove} -> IO.puts("remove cat #{name}")
    end
  end

  def handle3({:dog, name}, :add) do
    IO.puts("add dog #{name}")
  end
  def handle3({:dog, name}, :remove) do
    IO.puts("remove dog #{name}")
  end
  def handle3({:cat, name}, :add) do
    IO.puts("add cat #{name}")
  end
  def handle3({:cat, name}, :remove) do
    IO.puts("remove cat #{name}")
  end

end

animal1 = {:cat, "Tihon"}
animal2 = {:dog, "Woof"}

Lesson_03.Task_03_10_ControlFlow.handle(animal1, :add)
Lesson_03.Task_03_10_ControlFlow.handle(animal2, :remove)

Lesson_03.Task_03_10_ControlFlow.handle2(animal2, :add)
Lesson_03.Task_03_10_ControlFlow.handle2(animal1, :remove)

Lesson_03.Task_03_10_ControlFlow.handle3(animal1, :add)
Lesson_03.Task_03_10_ControlFlow.handle3(animal2, :add)