defmodule ControlFlow do

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

  def handle4(animal) do
    case animal do
      {:dog, name, age} when age > 10 -> IO.puts("#{name} is a dog older than 10")
      {:dog, name, _} -> IO.puts("#{name} is a 10 years old or younger dog")
      {:cat, name, age} when age > 10 -> IO.puts("#{name} is a cat older than 10")
      {:cat, name, _} -> IO.puts("#{name} is a 10 years old or younger cat")
    end
  end

  def handle5({:dog, name, age}) when age > 10 do
    IO.puts("#{name} is a dog older than 10")
  end
  def handle5({:dog, name, _age}) do
    IO.puts("#{name} is a 10 years old or younger dog")
  end
  def handle5({:cat, name, age}) when age > 10 do
    IO.puts("#{name} is a cat older than 10")
  end
  def handle5({:cat, name, _age}) do
    IO.puts("#{name} is a 10 years old or younger cat")
  end

  def handle6(num) when 10 / num > 2 do
    IO.puts("clause 1")
  end
  def handle6(num) do
    IO.puts("clause 2")
  end

  def handle7(num) do
    cond do
      num > 10 -> IO.puts("more than 10")
      num > 5 -> IO.puts("more than 5")
    end
  end

end

animal1 = {:cat, "Tihon"}
animal2 = {:dog, "Woof"}

alias Lesson_03.Task_03_10_ControlFlow, as: CF

CF.handle(animal1, :add)
CF.handle(animal2, :remove)

CF.handle2(animal2, :add)
CF.handle2(animal1, :remove)

CF.handle3(animal1, :add)
CF.handle3(animal2, :add)

CF.handle4({:dog, "Woof", 14})
CF.handle4({:dog, "Woof", 9})

CF.handle5({:cat, "Tihon", 12})
CF.handle5({:cat, "Tihon", 10})

CF.handle6(0)
CF.handle6(5)
CF.handle6(3)

CF.handle7(20)
CF.handle7(8)
CF.handle7(3)
