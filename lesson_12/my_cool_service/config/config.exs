import Config

config :logger, :console,
  level: :info

config :my_cool_service,
  agent_a_state: [
    { 0, 11, "Node-1"},
    {12, 23, "Node-2"},
    {24, 35, "Node-3"},
    {36, 47, "Node-4"}
  ],
  agent_b_state: [
    { 0, 7, "NodeA"},
    {8, 15, "NodeB"}
  ],
  cities_ru: "cities_ru.csv",
  cities_eu: "cities_en.csv"
  
import_config "#{Mix.env()}.exs"
