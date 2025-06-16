defmodule MyMusicBand.Vocalist do
  alias MyMusicBand.Model.Sound
  alias MyMusicBand.PlayerLogic

  @derive MyMusicBand.Player

  defstruct [:sounds, :current_stream]

  @type t() :: %__MODULE__{
          sounds: [Sound.vocal_sound()],
          current_stream: Enumerable.t()
        }

  @spec init([Sound.vocal_sound()]) :: {:ok, t()} | {:error, [{integer(), :atom}]}
  def init(sounds) do
    PlayerLogic.init(%__MODULE__{}, sounds, &Sound.is_vocal/1)
  end

  @spec next(t()) :: {atom(), t()}
  def next(vocalist), do: PlayerLogic.next(vocalist)
end
