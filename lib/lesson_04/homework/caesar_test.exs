ExUnit.start()

defmodule CaesarTest do
  use ExUnit.Case
  doctest Caesar
  import Caesar

  test "encode/decode" do
    assert encode("Hello", 10) |> decode(10) == "Hello"
    assert encode("12345", 1) == "23456"
    assert decode("12345", 1) == "01234"
    assert encode('abcdef', 2) == 'cdefgh'
    assert decode('abcdef', 2) == '_`abcd'
  end

  test "encode/decode with cyrillic symbols" do
    assert encode("Привет", 10) |> decode(10) == "Привет"
    assert encode('Привет мир', 500) |> decode(500) == 'Привет мир'
  end

  test "encode/decode ascii" do
    assert encode_ascii("Hello", 10) |> decode_ascii(10) == "Hello"
    assert encode_ascii("12345", 1) == "23456"
    assert decode_ascii("23456", 1) == "12345"
    assert encode_ascii('abcdef', 2) == 'cdefgh'
    assert decode_ascii('cdefgh', 2) == 'abcdef'
  end

  test "encode ascii with wrapping" do
    assert encode_ascii("xyz", 1) == "yz{"
    assert encode_ascii("xyz", 2) == "z{|"
    assert encode_ascii("xyz", 3) == "{|}"
    assert encode_ascii("xyz", 4) == "|}~"
    assert encode_ascii("xyz", 5) == "}~ "
    assert encode_ascii("xyz", 6) == "~ !"
    assert encode_ascii("xyz", 7) == " !\""
    assert encode_ascii("xyz", 8) == "!\"#"
    assert encode_ascii("xyz", 9) == "\"#$"
    assert encode_ascii("xyz", 10) == "#$%"
  end

  test "decode ascii with wrapping" do
    assert decode_ascii("yz{", 1) == "xyz"
    assert decode_ascii("z{|", 2) == "xyz"
    assert decode_ascii("{|}", 3) == "xyz"
    assert decode_ascii("|}~", 4) == "xyz"
    assert decode_ascii("}~ ", 5) == "xyz"
    assert decode_ascii("~ !", 6) == "xyz"
    assert decode_ascii(" !\"", 7) == "xyz"
    assert decode_ascii("!\"#", 8) == "xyz"
    assert decode_ascii("\"#$", 9) == "xyz"
    assert decode_ascii("#$%", 10) == "xyz"
  end

  test "invalid ascii str" do
    assert_raise RuntimeError, "invalid ascii str", fn -> encode_ascii("привет", 10) end
    assert_raise RuntimeError, "invalid ascii str", fn -> encode_ascii([31, 32, 33], 10) end
    assert_raise RuntimeError, "invalid ascii str", fn -> decode_ascii("привет", 10) end
    assert_raise RuntimeError, "invalid ascii str", fn -> decode_ascii([126, 127], 10) end
  end
end
