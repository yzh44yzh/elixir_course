defmodule Lesson_11 do

  defmodule MyService do

    def start() do
      children = [
        Lesson_11.AuthDataLoaderSup,
      ]
      Supervisor.start_link(children, strategy: :one_for_one) 
    end

    def update_auth_rules() do
      Lesson_11.AuthDataLoaderSup.start_child()
    end
    
  end
  
  defmodule AuthDataLoaderSup do
    
    use DynamicSupervisor

    def start_link(args) do
      DynamicSupervisor.start_link(__MODULE__, args, name: __MODULE__)
    end
    
    def start_child() do
      url = "http://auth_service.some_cluster.data_center/rules"
      spec = {Lesson_11.AuthDataLoader, [url]}
      DynamicSupervisor.start_child(__MODULE__, spec)
    end

    @impl true
    def init(_args) do
      DynamicSupervisor.init(strategy: :one_for_one)
    end
    
  end

  defmodule AuthDataLoader do

    use GenServer, restart: :transient
    
    def start_link(auth_service_url) do
      GenServer.start_link(__MODULE__, auth_service_url, [])
    end

    @impl true
    def init(auth_service_url) do
      IO.puts("worker #{inspect self()} started")
      {:ok, auth_service_url, {:continue, :delayed_init}}
    end

    @impl true
    def handle_continue(:delayed_init, auth_service_url) do
      load(auth_service_url) |> save()
      IO.puts("work done")
      {:stop, :normal, auth_service_url}
    end

    defp load(auth_service_url) do
      IO.puts("load data from #{auth_service_url}")
      Process.sleep(1000)
      [:rule_1, :rule_2, :rule_3]
    end

    defp save(data) do
      IO.puts("save data #{inspect data}")
    end
    
  end

end
