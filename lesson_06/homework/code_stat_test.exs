ExUnit.start()

defmodule CodeStatTest do
  use ExUnit.Case

  setup do
    tmp_dir = "./tmp"
    if File.exists?(tmp_dir), do: raise("#{tmp_dir} is already exists")

    File.mkdir(tmp_dir)
    make_test_dirs_and_files(tmp_dir)

    on_exit(fn ->
      File.rm_rf!(tmp_dir)
      :ok
    end)

    [tmp_dir: tmp_dir]
  end

  test "analyze", %{tmp_dir: tmp_dir} do
    expected = %{
      "Elixir" => %{files: 2, lines: 7, size: 58},
      "Erlang" => %{files: 1, lines: 3, size: 41},
      "Python" => %{files: 1, lines: 2, size: 13},
      "JavaScript" => %{files: 1, lines: 3, size: 32},
      "SQL" => %{files: 1, lines: 1, size: 3},
      "JSON" => %{files: 0, lines: 0, size: 0},
      "Web" => %{files: 0, lines: 0, size: 0},
      "Scripts" => %{files: 0, lines: 0, size: 0},
      "Configs" => %{files: 1, lines: 1, size: 11},
      "Docs" => %{files: 2, lines: 4, size: 32},
      "Other" => %{files: 2, lines: 2, size: 34}
    }

    assert expected == CodeStat.analyze(tmp_dir)
  end

  def make_test_dirs_and_files(tmp_dir) do
    File.mkdir!(tmp_dir <> "/aa")
    File.write!(tmp_dir <> "/aa/aa1.ex", "word1 word2\nword3 word4")
    File.write!(tmp_dir <> "/aa/aa2.erl", "word1 word2 word3\nword4 word5\nword6 word7")
    File.write!(tmp_dir <> "/aa/aa3.exs", "word1 word2\nword3\nword4\nword5\nword6")
    File.mkdir!(tmp_dir <> "/bb")
    File.write!(tmp_dir <> "/bb/bb1.py", "python\npython")
    File.write!(tmp_dir <> "/bb/bb2.js", "javascript\njavascript\njavascript")
    File.write!(tmp_dir <> "/cc1.sql", "sql")
    File.write!(tmp_dir <> "/cc2.yaml", "some config")
    File.write!(tmp_dir <> "/cc3.md", "doc3\nbla-bla-bla")
    File.write!(tmp_dir <> "/cc4.md", "doc4\nbla-bla-bla")
    File.write!(tmp_dir <> "/cc5.png", "binary image")
    File.write!(tmp_dir <> "/file", "file without extention")
    File.write!(tmp_dir <> "/cc6.log", "some log record")
  end
end
