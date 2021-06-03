# Custom exceptions

Exceptions in Elixir are basically records.
_Но records не описаны в книге Дейва Томаса._
все-таки struct?

MatchError is too generic; it can
happen for several reasons. It’s better to provide specific error structs to
clarify the problem by adding more context

You can define your own exceptions by creating a module.
Inside it, use defexception to define the various fields in the exception,
along with their default values.

Because you’re creating a module, you can also add functions —
often these are used to format the exception’s fields into meaningful messages.

```
defmodule KinectProtocolError do
  defexception message: "Kinect protocol error",
    can_retry: false

  def full_message(me) do
    "Kinect failed: #{me.message}, retriable: #{me.can_retry}"
  end
end
```

```
try do
  talk_to_kinect()
rescue
  error in [KinectProtocolError] ->
    IO.puts KinectProtocolError.full_message(error)
    if error.can_retry, do: schedule_retry()
end
```
