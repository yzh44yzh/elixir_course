ExUnit.start()

defmodule TrimTest do
  use ExUnit.Case
  doctest Trim
  import Trim

  test "trim binary" do
    assert trim("") == ""
    assert trim("a") == "a"
    assert trim(" ") == ""
    assert trim("   ") == ""
    assert trim("hello") == "hello"
    assert trim("  hello") == "hello"
    assert trim("hello  ") == "hello"
    assert trim("  hello  ") == "hello"
    assert trim("  hello  there  ") == "hello  there"
    assert trim("  check for  spaces   in  the middle  ") == "check for  spaces   in  the middle"
    assert trim(" Привет мир! ") == "Привет мир!"
    assert trim("ё") == "ё"
    assert trim(" ё ") == "ё"
    assert trim(" Ёу, проверим пробелы  в середине     сроки!   ") == "Ёу, проверим пробелы  в середине     сроки!" 
  end

  test "trim list" do
    assert trim('') == ''
    assert trim('a') == 'a'
    assert trim(' ') == ''
    assert trim('   ') == ''
    assert trim('hello') == 'hello'
    assert trim('  hello') == 'hello'
    assert trim('hello  ') == 'hello'
    assert trim('  hello  ') == 'hello'
    assert trim('  hello  there  ') == 'hello  there'
    assert trim('  check for  spaces   in  the middle  ') == 'check for  spaces   in  the middle'
    assert trim(' Привет мир! ') == 'Привет мир!'
    assert trim('ё') == 'ё'
    assert trim(' ё ') == 'ё'
    assert trim(' Ёу, проверим пробелы  в середине     сроки!   ') == 'Ёу, проверим пробелы  в середине     сроки!'
  end

  test "effective trim list" do
    assert effective_trim('') == ''
    assert effective_trim('a') == 'a'
    assert effective_trim(' ') == ''
    assert effective_trim('   ') == ''
    assert effective_trim('hello') == 'hello'
    assert effective_trim('  hello') == 'hello'
    assert effective_trim('hello  ') == 'hello'
    assert effective_trim('  hello  ') == 'hello'
    assert effective_trim('  hello  there  ') == 'hello  there'
    assert effective_trim('  check for  spaces   in  the middle  ') == 'check for  spaces   in  the middle'
    assert effective_trim(' Привет мир! ') == 'Привет мир!'
    assert effective_trim('ё') == 'ё'
    assert effective_trim(' ё ') == 'ё'
    assert effective_trim(' Ёу, проверим пробелы  в середине     сроки!   ') == 'Ёу, проверим пробелы  в середине     сроки!'
  end

  test "effective trim binary" do
    assert effective_trim("") == ""
    assert effective_trim("a") == "a"
    assert effective_trim(" ") == ""
    assert effective_trim("   ") == ""
    assert effective_trim("hello") == "hello"
    assert effective_trim("  hello") == "hello"
    assert effective_trim("hello  ") == "hello"
    assert effective_trim("  hello  ") == "hello"
    assert effective_trim("  hello  there  ") == "hello  there"
    assert effective_trim("  check for  spaces   in  the middle  ") == "check for  spaces   in  the middle"
    assert effective_trim(" Привет мир! ") == "Привет мир!"
    assert effective_trim("ё") == "ё"
    assert effective_trim(" ё ") == "ё"
    assert effective_trim(" Ёу, проверим пробелы  в середине     сроки!   ") == "Ёу, проверим пробелы  в середине     сроки!"
  end

end
