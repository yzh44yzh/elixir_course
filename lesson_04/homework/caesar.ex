defmodule Caesar do

  # We consider only chars in range 32 - 126 as valid ascii chars
  # http://www.asciitable.com/
  @min_ascii_char 32
  @max_ascii_char 126

  @doc """
  Function shifts forward all characters in string. String could be double-quoted or single-quoted.

  ## Examples
  iex> Caesar.encode("Hello", 10)
  "Rovvy"
  iex> Caesar.encode('Hello', 5)
  'Mjqqt'
  """
  def encode(str, code) do
    # TODO add your implementation
  end

  @doc """
  Function shifts backward all characters in string. String could be double-quoted or single-quoted.

  ## Examples
  iex> Caesar.decode("Rovvy", 10)
  "Hello"
  iex> Caesar.decode('Mjqqt', 5)
  'Hello'
  """
  def decode(str, code) do
    # TODO add your implementation
  end

  @doc ~S"""
  Function shifts forward all characters in string. String could be double-quoted or single-quoted.
  All characters should be in range 32-126, otherwise function raises RuntimeError with message "invalid ascii str"
  If result characters are out of valid range, than function wraps them to the beginning of the range.

  ## Examples
  iex> Caesar.encode_ascii('hello world', 15)
  'wt{{~/\'~\"{s'
  """
  def encode_ascii(str, code) do
    # TODO add your implementation
  end

  @doc ~S"""
  Function shifts backward all characters in string. String could be double-quoted or single-quoted.
  All characters should be in range 32-126, otherwise function raises RuntimeError with message "invalid ascii str"
  If result characters are out of valid range, than function wraps them to the end of the range.

  ## Examples
  iex> Caesar.decode_ascii('wt{{~/\'~\"{s', 15)
  'hello world'
  """
  def decode_ascii(str, code) do
    # TODO add your implementation
  end

end