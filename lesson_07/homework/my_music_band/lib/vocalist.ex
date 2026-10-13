defmodule MyMusicBand.Vocalist do
  use MyMusicBand.Model.Musician, validator: &MyMusicBand.Model.Sound.is_vocal/1
end
