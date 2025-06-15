defmodule MyMusicBand.Guitarist do
  alias MyMusicBand.Model.Sound
  import Sound, only: [is_guitar: 1]

  defstruct [:sounds, :current_stream]

  @type t() :: %__MODULE__{
          sounds: [Sound.guitar_sound()],
          current_stream: Enumerable.t()
        }

  @spec init([Sound.guitar_sound()]) :: {:ok, t()} | {:error, [{integer(), :atom}]}
  def init(sounds) do
    case validate_sounds(sounds) do
      ^sounds ->
        {:ok,
         %__MODULE__{
           sounds: sounds,
           current_stream: Stream.cycle(sounds)
         }}

      {:error, error} ->
        {:error, error}
    end
  end

  @spec next(t()) :: {Sound.guitar_sound(), t()}
  def next(%__MODULE__{current_stream: stream} = drummer) do
    {sound, new_stream} = get_sound(stream)
    drummer = %__MODULE__{drummer | current_stream: new_stream}
    {sound, drummer}
  end

  defp get_sound(stream) do
    case stream |> Stream.take(1) |> Enum.to_list() do
      [sound] ->
        {sound, stream |> Stream.drop(1)}

      _ ->
        {nil, stream}
    end
  end

  defp validate_sounds(sounds) do
    case Enum.with_index(sounds, 1)
         |> Enum.filter(fn {sound, _index} -> !is_guitar(sound) end)
         |> Enum.map(fn {sound, index} -> {index, sound} end) do
      [] ->
        sounds

      errors ->
        {:error, errors}
    end
  end
end
