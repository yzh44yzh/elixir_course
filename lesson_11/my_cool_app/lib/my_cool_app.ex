defmodule MyCoolApp do
  use Application

  @impl true
  def start(_start_type, _args) do
    data_file = Application.get_env(:my_cool_app, :data_file)
    data_file = Application.app_dir(:my_cool_app, "priv") |> Path.join(data_file)
    children = [
      {MyCoolApp.PathFinder, [data_file]}
    ]
    Supervisor.start_link(children, strategy: :one_for_one) 
  end
end
