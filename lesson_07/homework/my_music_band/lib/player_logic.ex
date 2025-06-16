defmodule MyMusicBand.PlayerLogic do
  @moduledoc """
  Contains common logic for all modules that implement Player behavior.
  """

  alias MyMusicBand.Player

  @spec init(struct(), list(atom), (atom() -> boolean())) ::
          {:ok, Player.t()} | {:error, list({pos_integer(), atom})}
  def init(struct, sounds, validator_fun) do
    errors =
      sounds
      |> Enum.with_index(1)
      |> Enum.reduce([], fn {sound, index}, acc ->
        if validator_fun.(sound), do: acc, else: [{index, sound} | acc]
      end)
      |> Enum.reverse()

    case errors do
      [] -> {:ok, %{struct | sounds: sounds, current_stream: Stream.cycle(sounds)}}
      _ -> {:error, errors}
    end
  end

  @spec next(Player.t()) :: {atom, Player.t()}
  def next(%{current_stream: stream} = player) do
    {sound, new_stream} = get_sound(stream)
    new_player = %{player | current_stream: new_stream}
    {sound, new_player}
  end

  defp get_sound(stream) do
    case stream |> Stream.take(1) |> Enum.to_list() do
      [sound] ->
        {sound, stream |> Stream.drop(1)}

      _ ->
        {nil, stream}
    end
  end
end
