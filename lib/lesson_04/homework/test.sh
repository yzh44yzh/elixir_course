#!/bin/sh

elixirc caesar.ex && elixir caesar_test.exs

elixirc trim.ex && elixir trim_test.exs

elixirc my_list.ex && elixir my_list_test.exs