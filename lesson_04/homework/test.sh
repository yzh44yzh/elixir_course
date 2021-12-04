#!/bin/sh

elixirc game.exs && elixir game_test.exs

elixirc tic_tac_toe.exs && elixir tic_tac_toe_test.exs
