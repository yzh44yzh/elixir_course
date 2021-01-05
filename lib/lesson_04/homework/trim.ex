defmodule Trim do

  # We only trim space character
  def is_space(char), do: char == 32

  @doc """
  Function trim spaces in the beginning and in the end of the string.
  It accepts both single-quoted and double-quoted strings.

  ## Examples
  iex> Trim.trim('   hello there   ')
  'hello there'
  iex> Trim.trim("  Привет   мир  ")
  "Привет   мир"
  """
  def trim(str) when is_list(str) do
    # We iterate string 4 times here
    str
    |> trim_left
    |> Enum.reverse
    |> trim_left
    |> Enum.reverse
  end

  def trim(str) when is_binary(str) do
    # And yet 2 more iterations here
    str
    |> to_charlist
    |> trim
    |> to_string
  end


  defp trim_left([]), do: []
  defp trim_left([head | tail] = str) do
    if is_space(head) do
      trim_left(tail)
    else
      str
    end
  end


  def effective_trim(str) do
    # Lets trim string with less than 4 iterations
    # TODO add your implementation
  end

end