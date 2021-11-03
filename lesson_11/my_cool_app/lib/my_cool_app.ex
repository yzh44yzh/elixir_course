defmodule MyCoolApp do
  use Application

  @impl true
  def start(_start_type, _args) do
    children = []
    Supervisor.start_link(children, strategy: :one_for_one) 
  end
end
