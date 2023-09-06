defmodule MailboxExample do
  def check_mailbox() do
    receive do
      {:tag1, msg} -> IO.puts("got msg with tag1: #{inspect(msg)}")
      {:tag2, msg} -> IO.puts("got msg with tag2: #{inspect(msg)}")
      msg -> IO.puts("got unknown msg #{inspect(msg)}")
    end
  end

  def check_mailbox(wait_time) do
    receive do
      msg -> IO.puts("got msg #{inspect(msg)}")
    after
      wait_time -> IO.puts("no messages after #{wait_time} ms")
    end
  end

  def check_for_42() do
    receive do
      42 -> IO.puts("got 42")
    after
      500 -> :ok
    end
  end
end
