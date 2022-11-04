defmodule MyCoolService do

  use Application
  require Logger

  @impl true
  def start(_start_type, _args) do
    {:ok, str} =  Jason.encode(%{a: 42})
    Logger.info("MyCoolApp.start #{str}")
    RootSup.start_link(:no_args)
  end
  
end
