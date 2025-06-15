defmodule MyMusicBand.Model.Sound do
  @moduledoc """
  Identifies valid sounds for each type of musician.
  """

  @type silence() :: :" "
  @silence :" "

  @type note_a() :: :"A-a-a"
  @type note_c() :: :"O-o-o"
  @type note_e() :: :"E-e-e"
  @type note_g() :: :Wooo
  @type vocal_sound() :: note_a() | note_c() | note_e() | note_g() | silence()
  @vocal_sounds [:"A-a-a", :"O-o-o", :"E-e-e", :Wooo, @silence]
  def is_vocal(sound), do: sound in @vocal_sounds

  @type accord_a() :: :A
  @type accord_d() :: :D
  @type accord_e() :: :E
  @type guitar_sound() :: accord_a() | accord_d() | accord_e() | silence()
  @guitar_sounds [:A, :D, :E, @silence]
  def is_guitar(sound), do: sound in @guitar_sounds

  @type kick() :: :BOOM
  @type snare() :: :Doom
  @type hi_hat() :: :Ts
  @type drum_sound() :: kick() | snare() | hi_hat() | silence()
  @drum_sound [:BOOM, :Doom, :Ts, @silence]
  def is_drum(sound), do: sound in @drum_sound

  @type sound() :: vocal_sound() | guitar_sound() | drum_sound()
end
