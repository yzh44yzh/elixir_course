defmodule MyMusicBand.Band do
  alias MyMusicBand.Player

  defstruct players: []

  @type t() :: %__MODULE__{
          players: list(Player.t())
        }

  @spec init() :: t()
  def init(), do: %__MODULE__{}

  @spec add_player(t(), Player.t()) :: t()
  def add_player(band, player) do
    %{band | players: band.players ++ [player]}
  end

  @spec next(t()) :: {list(atom), t()}
  def next(band) do
    {sounds, updated_players} =
      Enum.map(band.players, fn player ->
        Player.next(player)
      end)
      |> Enum.unzip()

    new_band = %{band | players: updated_players}
    {sounds, new_band}
  end
end
