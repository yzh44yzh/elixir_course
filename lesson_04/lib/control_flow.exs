defmodule ControlFlow do

  def handle(animal, action) do
    case animal do
      {:dog, name} ->
        case action do
          :feed -> IO.puts("feed the dog #{name}")
          :pet -> IO.puts("pet the dog #{name}")
        end
      {:cat, name} ->
        case action do
          :feed -> IO.puts("feed the cat #{name}")
          :pet -> IO.puts("pet the cat #{name}")
        end
    end
  end

  def handle2(animal, action) do
    case {animal, action} do
      {{:dog, name}, :feed} -> IO.puts("feed the dog #{name}")
      {{:dog, name}, :pet} -> IO.puts("pet the dog #{name}")
      {{:cat, name}, :feed} -> IO.puts("feed the cat #{name}")
      {{:cat, name}, :pet} -> IO.puts("pet the cat #{name}")
    end
  end

  def handle3({:dog, name}, :feed) do
    IO.puts("feed the dog #{name}")
  end
  def handle3({:dog, name}, :pet) do
    IO.puts("pet the dog #{name}")
  end
  def handle3({:cat, name}, :feed) do
    IO.puts("feed the cat #{name}")
  end
  def handle3({:cat, name}, :pet) do
    IO.puts("pet the cat #{name}")
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

alias ControlFlow, as: CF

CF.handle(animal1, :feed)
CF.handle(animal2, :pet)

CF.handle2(animal2, :feed)
CF.handle2(animal1, :pet)

CF.handle3(animal1, :feed)
CF.handle3(animal2, :feed)

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
