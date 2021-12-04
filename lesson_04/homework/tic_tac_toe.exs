defmodule TicTacToe do

  @type cell :: :x | :o | :f
  @type row :: {cell, cell, cell}
  @type game_state :: {row, row, row}
  @type game_result :: {:win, :x} | {:win, :o} | :no_win

  @spec valid_game?(game_state) :: boolean
  def valid_game?(state) do
    # TODO add your implementation
  end

  @spec check_who_win(game_state) :: game_result
  def check_who_win(state) do
    # TODO add your implementation
  end

end
