defmodule MyMusicBand.Guitarist do
  use MyMusicBand.Model.Musician, validator: &MyMusicBand.Model.Sound.is_guitar/1
end
