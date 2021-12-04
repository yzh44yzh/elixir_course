defmodule Lesson_07.Task_02_ExceptionTypes do

  def try_resque(exc_type) do
    try do
      generate_exception(exc_type)
    rescue
      error -> IO.puts("rescue from #{inspect error}")
    end
  end

  def try_catch(exc_type) do
    try do
      generate_exception(exc_type)
    catch
      err_type, error -> IO.puts("catch #{err_type} #{inspect error}")
    end
  end

  def generate_exception(:raise), do: raise "something went wrong" # TODO try different elixir exceptions
  def generate_exception(:throw), do: throw :something_went_wrong
  def generate_exception(:error), do: :erlang.error(:something_went_wrong)
  def generate_exception(:exit),  do: exit(:something_went_wrong)

end
