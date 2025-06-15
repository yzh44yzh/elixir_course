defmodule MyMusicBand.Band do
  alias MyMusicBand.{Vocalist, Guitarist, Drummer}

  defstruct players: []

  @type t() :: %__MODULE__{
          players: list(Guitarist.t() | Drummer.t() | Vocalist.t())
        }

  @spec init() :: t()
  def init(), do: %__MODULE__{}

  @spec add_player(t(), Guitarist.t() | Drummer.t() | Vocalist.t()) :: t()
  def add_player(band, player) do
    %{band | players: band.players ++ [player]}
  end

  @spec next(t()) :: {list(atom), t()}
  def next(band) do
    {sounds, updated_players} =
      Enum.map(band.players, fn player ->
        player.__struct__.next(player)
      end)
      |> Enum.unzip()

    new_band = %{band | players: updated_players}
    {sounds, new_band}
  end
end
