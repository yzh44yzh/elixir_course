defmodule EuropeCity do

  def population() do
    %{
      "Istanbul" => 15_462_452,
      "Moscow" => 12_195_221,
      "London" => 9_126_366,
      "Saint Petersburg" => 5_383_890,
      "Berlin" => 3_748_148,
      "Madrid" => 3_223_334,
      "Kyiv" => 2_950_800,
      "Rome" => 2_844_750,
      "Paris" => 2_140_526,
      "Minsk" => 1_982_444,
      "Vienna" => 1_921_153,
      "Hamburg" => 1_899_160,
      "Bucharest" => 1_832_883,
      "Warsaw" => 1_793_579,
      "Budapest" => 1_768_073,
      "Barcelona" => 1_636_762,
      "Munich" => 1_471_508,
      "Kharkiv" => 1_451_132,
      "Milan" => 1_404_239,
      "Belgrade" => 1_397_939
    }
  end

  def eu_cities() do
    [
      "Berlin",
      "Madrid",
      "Rome",
      "Bucharest",
      "Paris",
      "Vienna",
      "Hamburg",
      "Warsaw",
      "Budapest",
      "Barcelona",
      "Munich",
      "Milan",
      "Prague",
      "Sofia",
      "Cologne",
      "Stockholm",
      "Naples",
      "Turin",
      "Amsterdam",
      "Marseille",
      "Zagreb",
      "Copenhagen"
    ]
  end

end