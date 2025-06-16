defprotocol MyMusicBand.Player do
  @moduledoc """
  Define the protocol for all musicians in the band.
  """
  alias MyMusicBand.{Vocalist, Drummer, Guitarist}

  @type player() :: Guitarist.t() | Drummer.t() | Vocalist.t()

  @doc "Returns the next sound and the updated state"
  @spec next(player()) :: {atom(), player()}
  def next(player)
end

defimpl MyMusicBand.Player, for: Any do
  alias MyMusicBand.{Vocalist, Drummer, Guitarist, Player}

  @spec next(Player.player()) :: {atom(), Player.player()}
  def next(player) do
    MyMusicBand.PlayerLogic.next(player)
  end
end
