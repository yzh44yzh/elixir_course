# Конфигурация приложения

By default, the environment of an application is an empty list. In a Mix project's mix.exs file, you can set the :env key in application/0:
```
def application do
  [env: [db_host: "localhost"]]
end
```
Now, in your application, you can read this environment by using functions such as fetch_env!/2 and friends:
Application.fetch_env!(:my_app, :db_host)

In Mix projects, the environment of the application and its dependencies can be overridden via the config/config.exs file.
```
import Config
config :my_app, :db_host, "db.local"
```
if you change the value of the application environment after the code is compiled, the value used at runtime is not going to change!

You can provide values through config script files.
Config scripts are evaluated before project is compiled and started.
Generated sys.config are baked into OTP release.
So build machine makes the same configuration for all prod machines.
Config scripts can't provide parameters from external sources, such as OS environment, ini-files, etcd, or vault.


можно передавать аргументы из mix.ex в app:
```
# mix.ex
def application do
  [
    mod: {
      Sequence.Application, 456
    },
    extra_applications: [:logger],
  ]
end

defmodule Sequence.Application do
  use Application
  def start(_type, initial_number) do
    ..
  end
end
```

### Diff configuration for test env

config.exs
```
use Mix.Config
config :my_app, param1: "value_default"

import config "#{Mix.env()}.exs"
```

dev.exs
```
use Mix.Config
config :my_app, param1: "value_dev"
```

test.exs
```
use Mix.Config
config :my_app, param1: "value_test"
```

config.exs provides common settings for all env. Other configs may override it.


### Кастомизация конфигурации для разных машин

TODO 


