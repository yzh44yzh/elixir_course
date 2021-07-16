defmodule Lesson_09.Task_02_Mailbox do

  def check_mailbox() do
    receive do
      {:tag1, msg} -> IO.puts("got msg with tag1: #{inspect msg}")
      {:tag2, msg} -> IO.puts("got msg with tag2: #{inspect msg}")
      msg -> IO.puts("got unknown msg #{inspect msg}")
    end
  end
  
end
