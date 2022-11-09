defmodule PathFinderService do
  use Application

  @impl true
  def start(_, _) do
    IO.puts("PathFinderService.start")
    RootSup.start_link(:no_args)
  end

end
