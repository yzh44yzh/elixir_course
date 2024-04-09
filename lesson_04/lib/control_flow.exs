defmodule ControlFlow do

  # inner case
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
      {:rat, name} ->
        case action do
          :feed -> IO.puts("feed the rat #{name}")
          :pet -> IO.puts("pet the rat #{name}")
        end
    end
  end

  # flat case
  def handle2(animal, action) do
    case {animal, action} do
      {{:dog, name}, :feed} -> IO.puts("feed the dog #{name}")
      {{:dog, name}, :pet} -> IO.puts("pet the dog #{name}")
      {{:cat, name}, :feed} -> IO.puts("feed the cat #{name}")
      {{:cat, name}, :pet} -> IO.puts("pet the cat #{name}")
      {{:rat, name}, :feed} -> IO.puts("feed the rat #{name}")
      {{:rat, name}, :pet} -> IO.puts("pet the rat #{name}")
    end
  end

  # function clause instead of case
  def handle3({:dog, name}, :feed), do: IO.puts("feed the dog #{name}")
  def handle3({:dog, name}, :pet), do: IO.puts("pet the dog #{name}")
  def handle3({:cat, name}, :feed), do: IO.puts("feed the cat #{name}")
  def handle3({:cat, name}, :pet), do: IO.puts("pet the cat #{name}")
  def handle3({:rat, name}, action) do
    IO.puts("do action '#{action}' with rat #{name}")
  end
  # catch all
  def handle3({animal_type, name}, action) do
    IO.puts("do action '#{inspect(action)}' with animal '#{inspect(animal_type)}' #{name}")
  end
  # bad catch all
  # def handle3(anything, whatever) do
  #   IO.puts("do '#{inspect(anything)}' with #{inspect(whatever)}")
  # end

  # guards with case
  def handle4(animal) do
    case animal do
      {:dog, name, age} when age <= 2 -> IO.puts("#{name} is a young dog")
      {:dog, name, _} -> IO.puts("#{name} is an adult dog")
      {:cat, name, age} when age > 10 -> IO.puts("#{name} is an old cat")
      {:cat, name, _} -> IO.puts("#{name} is not so old")
    end
  end

  # guards with function clauses
  def handle5({:dog, name, age}) when age <= 2 do
    IO.puts("#{name} is a young dog")
  end
  def handle5({:dog, name, _age}) do
    IO.puts("#{name} is an adult dog")
  end
  def handle5({:cat, name, age}) when age > 10 do
    IO.puts("#{name} is an old cat")
  end
  def handle5({:cat, name, _age}) do
    IO.puts("#{name} is not so old")
  end

  # sequence of guards
  def handle6({:library, rating, books}) when rating > 4 and length(books) > 2 do
    IO.puts("good library")
  end

  def handle6({:library, rating, books}) when rating > 4 or length(books) > 2 do
    IO.puts("not too bad")
  end

  def handle6({:library, _rating, _books}) do
    IO.puts("not recommended")
  end

  # invalid guard
  # def handle7(a) when handle6(a) > 5 do
  #   :ok
  # end

  # macros in guard
  require Integer

  def handle8(num) when Integer.is_even(num) do
    IO.puts("#{num} is even")
  end

  def handle8(num) when Integer.is_odd(num) do
    IO.puts("#{num} is odd")
  end

  # crash in guards
  def handle9(a, b) when 10 / a > 2 do
    {:clause_1, b}
  end

  def handle9(_a, b) do
    {:clause_2, 10 / b}
  end

  # def handle10(a) when is_map(a) and map_size(a) > 2 do
  def handle10(a) when map_size(a) > 2 do
    IO.puts("a big map")
  end

  def handle10(_a) do
    IO.puts("not a big map")
  end

  # cond
  def handle11(num) do
    cond do
      num > 10 -> IO.puts("more than 10")
      num > 5 -> IO.puts("more than 5")
    end
  end

  # cond catch all
  def handle12(num) do
    cond do
      num > 10 -> IO.puts("more than 10")
      num > 5 -> IO.puts("more than 5")
      true -> IO.puts("less or equal to 5")
    end
  end

  # order
  def handle13(num) do
    cond do
      num > 5 -> IO.puts("more than 5")
      num > 10 -> IO.puts("more than 10")
      true -> IO.puts("less or equal to 5")
    end
  end

  # cond to if
  def handle14(num) do
    cond do
      num >= 5 -> IO.puts("more or equal to 5")
      true -> IO.puts("less than 5")
    end
  end

  # if
  def handle15(num) do
    if num >= 5 do
      IO.puts("more or equal to 5")
    else
      IO.puts("less than 5")
    end
  end

  # one guard cond
  def handle16(num) do
    cond do
      num >= 5 -> IO.puts("more or equal to 5")
    end
  end

  # return value
  def handle17(num) do
    if num >= 5 do
      :more_or_equal_to_5
    else
      :less_than_5
    end
  end

  def handle18(num) do
    if num >= 5 do
      :more_or_equal_to_5
    end
  end

end

# animal_1 = {:cat, "Tihon"}
# animal_2 = {:dog, "Woof"}
# animal_3 = {:rat, "Bet"}

# alias ControlFlow, as: CF

# CF.handle(animal1, :feed)
# CF.handle(animal2, :pet)

# CF.handle2(animal2, :feed)
# CF.handle2(animal1, :pet)

# CF.handle3(animal1, :feed)
# CF.handle3(animal2, :feed)

# CF.handle4({:dog, "Woof", 14})
# CF.handle4({:dog, "Woof", 9})

# CF.handle5({:cat, "Tihon", 12})
# CF.handle5({:cat, "Tihon", 10})

# CF.handle6(0)
# CF.handle6(5)
# CF.handle6(3)

# CF.handle7(20)
# CF.handle7(8)
# CF.handle7(3)
