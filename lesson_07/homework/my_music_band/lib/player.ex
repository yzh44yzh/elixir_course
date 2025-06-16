defmodule MyMusicBand.Player do
  @moduledoc """
  Defines the behavior for all musicians in the band.
  """

  @type t :: %{sounds: list(atom), current_stream: Enumerable.t()}

  @doc "Initializes the state of the musician with his part"
  @callback init(list(atom)) :: {:ok, t()} | {:error, any()}

  @doc "Returns the next sound and the updated state"
  @callback next(t()) :: {atom(), t()}
end
