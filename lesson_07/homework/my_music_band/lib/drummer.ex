defmodule MyMusicBand.Drummer do
  @behaviour MyMusicBand.Player

  alias MyMusicBand.Model.Sound
  alias MyMusicBand.PlayerLogic

  defstruct [:sounds, :current_stream]

  @type t() :: %__MODULE__{
          sounds: [Sound.drum_sound()],
          current_stream: Enumerable.t()
        }

  @impl true
  @spec init([Sound.drum_sound()]) :: {:ok, t()} | {:error, [{integer(), :atom}]}
  def init(sounds) do
    PlayerLogic.init(%__MODULE__{}, sounds, &Sound.is_drum/1)
  end

  @impl true
  def next(drummer) do
    PlayerLogic.next(drummer)
  end
end
