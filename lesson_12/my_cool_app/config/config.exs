import Config

config :my_cool_app,
  data_file: "cities.csv",
  sharding: %{
    agent_a: [
      { 0, 11, "Node-1"},
      {12, 23, "Node-2"},
      {24, 35, "Node-3"},
      {36, 47, "Node-4"}
    ],
    agent_b: [
      { 0,  7, "Node-1"},
      { 8, 15, "Node-2"},
      {16, 23, "Node-3"},
      {24, 31, "Node-4"}
    ]
  }
