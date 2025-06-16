defmodule MyMusicBand.Model.Musician do
  @moduledoc """
  Defines the behavior for all musicians in the band.
  """
  defmacro __using__(opts) do
    validator_fun = Keyword.fetch!(opts, :validator)

    quote do
      alias MyMusicBand.Model.Sound
      @type t :: %{sounds: list(atom), current_stream: Enumerable.t()}
      defstruct [:sounds, :current_stream]

      @spec init(Sound.sound()) ::
              {:ok, t()} | {:error, {non_neg_integer(), atom()}}
      def init(sounds) do
        case validate_sounds(sounds) do
          [] -> {:ok, %__MODULE__{sounds: sounds, current_stream: Stream.cycle(sounds)}}
          errors_sounds -> {:error, errors_sounds}
        end
      end

      @spec next(t()) :: {Sound.sound(), t()}
      def next(%{current_stream: stream} = musician) do
        {sound, new_stream} = get_sound(stream)
        new_musician = %{musician | current_stream: new_stream}
        {sound, new_musician}
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
        sounds
        |> Enum.with_index(1)
        |> Enum.reduce([], fn {sound, index}, acc ->
          if unquote(validator_fun).(sound), do: acc, else: [{index, sound} | acc]
        end)
        |> Enum.reverse()
      end
    end
  end
end
