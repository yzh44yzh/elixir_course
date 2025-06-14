defmodule MyMusicBand.Model.Sound do

  @type silence() :: :' '

  @type note_a() :: :'A-a-a'
  @type note_c() :: :'O-o-o'
  @type note_e() :: :'E-e-e'
  @type note_g() :: :'Wooo'
  @type vocal_sound() :: note_a() | note_c() | note_e() | note_g() | silence()

  @type accord_a() :: :A
  @type accord_d() :: :D
  @type accord_e() :: :E
  @type guitar_sound:: accord_a() | accord_d() | accord_e() | silence()

  @type kick() :: :BOOM
  @type snare() :: :Doom
  @type hi_hat() :: :Ts
  @type drum_sound :: kick() | snare() | hi_hat() | silence()

  @type sound() :: vocal_sound() | guitar_sound() | drum_sound()

end
