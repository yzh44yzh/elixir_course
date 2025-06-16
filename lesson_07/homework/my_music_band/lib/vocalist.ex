defmodule MyMusicBand.Vocalist do
  @behaviour MyMusicBand.Player

  alias MyMusicBand.Model.Sound
  alias MyMusicBand.PlayerLogic

  defstruct [:sounds, :current_stream]

  @type t() :: %__MODULE__{
          sounds: [Sound.vocal_sound()],
          current_stream: Enumerable.t()
        }

  @impl true
  @spec init([Sound.vocal_sound()]) :: {:ok, t()} | {:error, [{integer(), :atom}]}
  def init(sounds) do
    PlayerLogic.init(%__MODULE__{}, sounds, &Sound.is_vocal/1)
  end

  @impl true
  def next(vocalist) do
    PlayerLogic.next(vocalist)
  end
end
