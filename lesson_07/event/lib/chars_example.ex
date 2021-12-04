defmodule CharsExample do

  defstruct [:a, :b]

  defimpl String.Chars do

    def to_string(data) do
      "#CharsExample<a=#{inspect data.a}, b=#{inspect data.b}>"
    end

  end

end

defimpl String.Chars, for: Map do

  def to_string(map) do
    "#Map of size:#{map_size(map)}"
  end

end
