defmodule MyCoolApp do
  use Application

  @impl true
  def start(_start_type, _args) do
    data_file = Application.get_env(:my_cool_app, :data_file)
    data_file = Application.app_dir(:my_cool_app, "priv") |> Path.join(data_file)

    sharding = Application.get_env(:my_cool_app, :sharding)
    
    children = [
      {MyCoolApp.PathFinder, [data_file]},
      %{
        id: :agent_a,
        start: {MyCoolApp.ShardingAgent, :start_link, [{:agent_a, sharding.agent_a}]}
      },
      %{
        id: :agent_b,
        start: {MyCoolApp.ShardingAgent, :start_link, [{:agent_b, sharding.agent_b}]}
      }
    ]
    Supervisor.start_link(children, strategy: :one_for_one) 
  end
end
