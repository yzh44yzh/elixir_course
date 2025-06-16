defmodule MyMusicBand.Drummer do
  alias MyMusicBand.Model.Sound
  alias MyMusicBand.PlayerLogic

  @derive MyMusicBand.Player

  defstruct [:sounds, :current_stream]

  @type t() :: %__MODULE__{
          sounds: [Sound.drum_sound()],
          current_stream: Enumerable.t()
        }

  @spec init([Sound.drum_sound()]) :: {:ok, t()} | {:error, [{integer(), :atom}]}
  def init(sounds) do
    PlayerLogic.init(%__MODULE__{}, sounds, &Sound.is_drum/1)
  end

  @spec next(t()) :: {atom(), t()}
  def next(drummer), do: PlayerLogic.next(drummer)
end
