defmodule MyMusicBand.Guitarist do
  alias MyMusicBand.Model.Sound
  alias MyMusicBand.PlayerLogic

  @derive MyMusicBand.Player

  defstruct [:sounds, :current_stream]

  @type t() :: %__MODULE__{
          sounds: [Sound.guitar_sound()],
          current_stream: Enumerable.t()
        }

  @spec init([Sound.guitar_sound()]) :: {:ok, t()} | {:error, [{integer(), :atom}]}
  def init(sounds) do
    PlayerLogic.init(%__MODULE__{}, sounds, &Sound.is_guitar/1)
  end

  @spec next(t()) :: {atom(), t()}
  def next(guitaris), do: PlayerLogic.next(guitaris)
end
