defmodule RootSup do
  use Supervisor

  require Logger

  def start_link(:no_args) do
    Logger.warning("RootSup.start_link")
    Supervisor.start_link(__MODULE__, :no_args)
  end

  @impl true
  def init(:no_args) do
    priv_dir =  Application.app_dir(:path_finder_service, "priv")
    data_file = Application.get_env(:path_finder_service, :data_file)
    data_file = Path.join(priv_dir, data_file)

    shard_manager_1 = Application.get_env(:path_finder_service, :shard_manager_1)
    shard_manager_2 = Application.get_env(:path_finder_service, :shard_manager_2)

    child_spec = [
      {PathFinder, data_file},
      %{
        id: :shard_manager_1,
        start: {ShardManager, :start_link, [{:shard_manager_1, shard_manager_1}]}
      },
      %{
        id: :shard_manager_2,
        start: {ShardManager, :start_link, [{:shard_manager_2, shard_manager_2}]}
      }
    ]
    Logger.info("RootSup.init #{inspect child_spec}")
    Supervisor.init(child_spec, strategy: :one_for_one)
  end
end
