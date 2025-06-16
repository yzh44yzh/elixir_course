defmodule MyMusicBand.Guitarist do
  @behaviour MyMusicBand.Player

  alias MyMusicBand.Model.Sound
  alias MyMusicBand.PlayerLogic

  defstruct [:sounds, :current_stream]

  @type t() :: %__MODULE__{
          sounds: [Sound.guitar_sound()],
          current_stream: Enumerable.t()
        }

  @impl true
  @spec init([Sound.guitar_sound()]) :: {:ok, t()} | {:error, [{integer(), :atom}]}
  def init(sounds) do
    PlayerLogic.init(%__MODULE__{}, sounds, &Sound.is_guitar/1)
  end

  @impl true
  def next(guitaris) do
    PlayerLogic.next(guitaris)
  end
end
