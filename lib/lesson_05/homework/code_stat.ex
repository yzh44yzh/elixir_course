defmodule CodeStat do

  @types [
      {"Elixir", [".ex", ".exs"]},
      {"Erlang", [".erl"]},
      {"Python", [".py"]},
      {"JavaScript", [".js"]},
      {"SQL", [".sql"]},
      {"JSON", [".json"]},
      {"Web", [".html", ".htm", ".css"]},
      {"Scripts", [".sh", ".lua", ".j2"]},
      {"Configs", [".yaml", ".yml", ".conf", ".args", ".env"]},
      {"Docs", [".md"]}
    ]

  @ignore_names [".git", ".gitignore", ".idea", "_build", "deps", "log", "tmp", ".formatter.exs"]

  @ignore_extensions [".beam", ".lock", ".iml", ".log", ".pyc"]

  @max_depth 5

  def analyze(path) do
    # TODO add your implementation
  end

end