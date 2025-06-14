defmodule MyMusicBandTest do
  use ExUnit.Case
  alias MyMusicBand.Band
  alias MyMusicBand.Vocalist
  alias MyMusicBand.Guitarist
  alias MyMusicBand.Drummer

  test "play Drummer sounds" do
    sounds = [:BOOM, :Ts, :Doom, :Ts]
    {:ok, drummer} = Drummer.init(sounds)

    {:BOOM, drummer} = Drummer.next(drummer)
    {:Ts, drummer} = Drummer.next(drummer)
    {:Doom, drummer} = Drummer.next(drummer)
    {:Ts, drummer} = Drummer.next(drummer)
    {:BOOM, drummer} = Drummer.next(drummer)
    {:Ts, drummer} = Drummer.next(drummer)
    {:Doom, _drummer} = Drummer.next(drummer)
  end

  test "play Guitarist sounds" do
    sounds = [:A, :E, :' ', :A, :D, :A]
    {:ok, guitarist} = Guitarist.init(sounds)

    {:A, guitarist} = Guitarist.next(guitarist)
    {:E, guitarist} = Guitarist.next(guitarist)
    {:' ', guitarist} = Guitarist.next(guitarist)
    {:A, guitarist} = Guitarist.next(guitarist)
    {:D, guitarist} = Guitarist.next(guitarist)
    {:A, guitarist} = Guitarist.next(guitarist)
    {:A, guitarist} = Guitarist.next(guitarist)
    {:E, guitarist} = Guitarist.next(guitarist)
    {:' ', _guitarist} = Guitarist.next(guitarist)
  end

  test "play Vocalist sounds" do
    sounds = [:'A-a-a', :' ', :'O-o-o', :'Wooo']
    {:ok, vocalist} = Vocalist.init(sounds)

    {:'A-a-a', vocalist} = Vocalist.next(vocalist)
    {:' ', vocalist} = Vocalist.next(vocalist)
    {:'O-o-o', vocalist} = Vocalist.next(vocalist)
    {:'Wooo', vocalist} = Vocalist.next(vocalist)
    {:'A-a-a', vocalist} = Vocalist.next(vocalist)
    {:' ', vocalist} = Vocalist.next(vocalist)
    {:'O-o-o', _vocalist} = Vocalist.next(vocalist)
  end

  test "play Band sounds" do
    {:ok, vocalist} = Vocalist.init([
      :'A-a-a', :'O-o-o', :' ', 
      :'A-a-a', :'Wooo', :' ',
      :'E-e-e', :'O-o-o', :'Wooo', 
      :' ', :'O-o-o', :'Wooo'
    ])
    {:ok, guitarist} = Guitarist.init([
      :A, :D, :' ', 
      :A, :E, :' ', 
      :E, :D, :A
    ])
    {:ok, drummer} = Drummer.init([
      :BOOM, :Ts, :Ts, 
      :Doom, :Ts, :Ts
    ])

    band = 
      Band.init()
      |> Band.add_player(vocalist)
      |> Band.add_player(guitarist)
      |> Band.add_player(drummer)

    {[:'A-a-a', :A, :BOOM], band} = Band.next(band)
    {[:'O-o-o', :D, :Ts], band} = Band.next(band)
    {[:' ', :' ', :Ts], band} = Band.next(band)
    {[:'A-a-a', :A, :Doom], band} = Band.next(band)
    {[:'Wooo', :E, :Ts], band} = Band.next(band)
    {[:' ', :' ', :Ts], band} = Band.next(band)
    {[:'E-e-e', :E, :BOOM], band} = Band.next(band)
    {[:'O-o-o', :D, :Ts], band} = Band.next(band)
    {[:'Wooo', :A, :Ts], band} = Band.next(band)
    {[:' ', :A, :Doom], band} = Band.next(band)
    {[:'O-o-o', :D, :Ts], band} = Band.next(band)
    {[:'Wooo', :' ', :Ts], band} = Band.next(band)
    {[:'A-a-a', :A, :BOOM], band} = Band.next(band)
    {[:'O-o-o', :E, :Ts], _band} = Band.next(band)
  end

  test "init Drummer with invalid sounds" do
    sounds = [:BOOM, :Ts, :Doom, :BoBoom, :Ts, :Ts, :Woooom, :Ts]
    {:error, [{4, :BoBoom}, {7, :Woooom}]} = Drummer.init(sounds)
  end

  test "init Guitarist with invalid sounds" do
    sounds = [:A, :G, :D, :' ']
    {:error, [{2, :G}]} = Guitarist.init(sounds)
  end

  test "init Vocalist with invalid sounds" do
    sounds = [:'A-a-a', :'Ops', :'GRHHH', :'U-u-u', :'Wooo', :' ', :'A-a-a']
    {:error, [{2, :'Ops'}, {3, :'GRHHH'}, {4, :'U-u-u'}]} = Vocalist.init(sounds)
  end
end
