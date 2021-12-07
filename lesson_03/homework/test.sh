#!/bin/sh

elixirc quadratic_equation.ex && elixir quadratic_equation_test.exs

elixirc rect.ex && elixir rect_test.exs

elixirc word_count.ex && elixir word_count_test.exs
