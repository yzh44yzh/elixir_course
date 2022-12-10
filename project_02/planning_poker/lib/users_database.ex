defmodule PlanningPoker.UsersDatabase do
  alias PlanningPoker.Model.User

  def get_users() do
    [
      %User{id: 1, name: "Yura", role: :participant},
      %User{id: 2, name: "Bob", role: :participant},
      %User{id: 3, name: "Helen", role: :leader},
      %User{id: 4, name: "Kate", role: :participant}
    ]
  end

  def get_by_name(name) do
    get_users()
    |> Enum.filter(fn user -> user.name == name end)
    |> case do
      [user] -> {:ok, user}
      [] -> {:error, :not_found}
    end
  end
end
