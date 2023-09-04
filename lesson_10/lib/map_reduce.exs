defmodule Map_ReduceExample do
  def start() do
    Map_ReduceExample.Coordinator.start()
  end

  defmodule Coordinator do
    alias Map_ReduceExample.Mapper
    alias Map_ReduceExample.Reducer

    def start() do
      processes_tree =
        {:reducer, :root_reducer,
         [
           {:reducer, :r1,
            [
              {:mapper, :w1, "./10_01_processes.md"},
              {:mapper, :w2, "./10_02_mailbox.md"}
            ]},
           {:reducer, :r2,
            [
              {:mapper, :w3, "./10_03_link.md"},
              {:mapper, :w4, "./10_04_monitor.md"}
            ]}
         ]}

      start(self(), processes_tree)

      receive do
        {:result, :root_reducer, result} ->
          {:ok, result}

        msg ->
          {:error, :unknown_msg, msg}
      after
        5000 ->
          {:error, :timeout}
      end
    end

    defp start(parent, {:reducer, id, childs}) do
      child_ids = Enum.map(childs, fn {_, id, _} -> id end)
      pid = spawn(Reducer, :start, [parent, id, child_ids])
      for child <- childs, do: start(pid, child)
    end

    defp start(parent, {:mapper, id, file}) do
      spawn(Mapper, :start, [parent, id, file])
    end
  end

  defmodule Mapper do
    def start(parent, id, file) do
      IO.puts("start mapper '#{id}' with file '#{file}'")
      result = process(file)
      send(parent, {:result, id, result})
    end

    defp process(file) do
      {:ok, content} = File.read(file)
      String.split(content) |> length()
    end
  end

  defmodule Reducer do
    def start(parent, id, child_ids) do
      IO.puts("start reducer '#{id}' with childs #{inspect(child_ids)}")
      result = wait_for_results(id, child_ids, 0)
      send(parent, {:result, id, result})
    end

    defp wait_for_results(_id, [], acc) do
      acc
    end

    defp wait_for_results(id, child_ids, acc) do
      receive do
        {:result, child_id, result} ->
          IO.puts("reducer #{id} got result #{result} from #{child_id}")
          wait_for_results(id, List.delete(child_ids, child_id), acc + result)

        msg ->
          IO.puts("got unknown msg #{inspect(msg)}")
          wait_for_results(id, child_ids, acc)
      after
        5000 ->
          IO.puts("reducer #{id} hasn't got all results")
      end
    end
  end
end
