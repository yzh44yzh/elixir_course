#!/bin/sh

elixirc world_champ.ex && elixir world_champ_test.exs

elixirc code_stat.ex && elixir code_stat_test.exs