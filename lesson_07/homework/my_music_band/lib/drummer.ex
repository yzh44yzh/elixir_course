defmodule MyMusicBand.Drummer do
  use MyMusicBand.Model.Musician, validator: &MyMusicBand.Model.Sound.is_drum/1
end
