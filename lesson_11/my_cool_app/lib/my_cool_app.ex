defmodule MyCoolApp do
  use Application

  @impl true
  def start(_start_type, start_args) do
    {:ok, self()}
  end
end
