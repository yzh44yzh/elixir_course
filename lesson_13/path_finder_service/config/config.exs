import Config

config :path_finder_service,
  data_file: "cities.csv",
  shard_manager_1: [
      { 0, 11, "Node-1"},
      {12, 23, "Node-2"},
      {24, 35, "Node-3"},
      {36, 47, "Node-4"}
    ],
  shard_manager_2: [
      { 0, 11, "Node-a"},
      {12, 23, "Node-b"},
      {24, 35, "Node-c"}
    ]

