ExUnit.start()

defmodule WorldChampTest do
  use ExUnit.Case
  import WorldChamp

  test "get stat" do
    champ = sample_champ()
    assert {5,40,24.85,242.8} == get_stat(champ)
  end


  test "examine champ" do
    champ = sample_champ()
    result = [
      {:team, "Crazy Bulls",
        [
          {:player, "Big Bull", 22, 545, 99},
          {:player, "Small Bull", 18, 324, 95},
          {:player, "Bill The Bull", 23, 132, 85},
          {:player, "Tall Ball Bull", 38, 50, 50},
          {:player, "Bull Dog", 35, 201, 91},
          {:player, "Bull Tool", 29, 77, 96},
          {:player, "Mighty Bull", 22, 145, 98}
        ]},
      {:team, "Fast Cows",
        [
          {:player, "Cow Bow", 28, 89, 90},
          {:player, "Boom! Cow", 20, 131, 99},
          {:player, "Light Speed Cow", 21, 201, 98},
          {:player, "Big Horn", 23, 38, 93},
          {:player, "Milky", 25, 92, 95},
          {:player, "Jumping Cow", 19, 400, 98}
        ]},
      {:team, "Extinct Monsters",
        [
          {:player, "T-Rex", 21, 999, 99},
          {:player, "Velociraptor", 29, 656, 99},
          {:player, "Giant Mammoth", 30, 382, 99},
          {:player, "The Big Croc", 42, 632, 99},
          {:player, "Huge Pig", 18, 125, 98},
          {:player, "Saber-Tooth", 19, 767, 97},
          {:player, "Beer Bear", 24, 241, 99}
        ]}
    ]
    assert result == examine_champ(champ)
  end


  test "make pairs" do
    [t1, t2, t3, t4, t5] = sample_champ()
    assert [
             {"Big Bull", "Lazy Horse"},
             {"Big Bull", "Sleepy Horse"},
             {"Big Bull", "Horse Doors"},
             {"Big Bull", "Rainbow"},
             {"Big Bull", "HoHoHorse"},
             {"Big Bull", "Pony"},
             {"Big Bull", "Hippo"},
             {"Big Bull", "Hop-Hop"},
             {"Small Bull", "Lazy Horse"},
             {"Bull Dog", "Lazy Horse"}
           ] == make_pairs(t1, t2)
    assert [
             {"Lazy Horse", "Light Speed Cow"},
             {"Lazy Horse", "Jumping Cow"},
             {"Lazy Horse", "Cow Flow"}
           ] == make_pairs(t2, t3)
    assert [
             {"Ben The Hen", "Light Speed Cow"},
             {"Ben The Hen", "Jumping Cow"},
             {"Ben The Hen", "Cow Flow"},
             {"Hen Hen", "Jumping Cow"},
             {"Hen Hen", "Cow Flow"},
             {"Son of Hen", "Boom! Cow"},
             {"Son of Hen", "Light Speed Cow"},
             {"Son of Hen", "Jumping Cow"},
             {"Son of Hen", "Cow Flow"}
           ] == make_pairs(t4, t3)
    assert [
             {"Lazy Horse", "T-Rex"},
             {"Lazy Horse", "Velociraptor"},
             {"Lazy Horse", "Giant Mammoth"},
             {"Lazy Horse", "The Big Croc"},
             {"Lazy Horse", "Saber-Tooth"},
             {"Lazy Horse", "Beer Bear"},
             {"Sleepy Horse", "T-Rex"},
             {"Sleepy Horse", "Velociraptor"},
             {"Sleepy Horse", "The Big Croc"},
             {"Sleepy Horse", "Saber-Tooth"},
             {"Horse Doors", "T-Rex"},
             {"Horse Doors", "Velociraptor"},
             {"Horse Doors", "The Big Croc"},
             {"Horse Doors", "Saber-Tooth"},
             {"Rainbow", "T-Rex"},
             {"Rainbow", "Velociraptor"},
             {"Rainbow", "The Big Croc"},
             {"Rainbow", "Saber-Tooth"},
             {"HoHoHorse", "T-Rex"},
             {"HoHoHorse", "Velociraptor"},
             {"HoHoHorse", "The Big Croc"},
             {"HoHoHorse", "Saber-Tooth"},
             {"Pony", "T-Rex"},
             {"Pony", "Velociraptor"},
             {"Pony", "The Big Croc"},
             {"Pony", "Saber-Tooth"},
             {"Hippo", "T-Rex"},
             {"Hippo", "Velociraptor"},
             {"Hippo", "The Big Croc"},
             {"Hippo", "Saber-Tooth"},
             {"Hop-Hop", "T-Rex"},
             {"Hop-Hop", "Velociraptor"},
             {"Hop-Hop", "The Big Croc"},
             {"Hop-Hop", "Saber-Tooth"}
           ] == make_pairs(t2, t5)
  end

end
