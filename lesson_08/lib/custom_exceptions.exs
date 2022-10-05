defmodule Lesson08.CustomExceptions do

  alias Model, as: M
  
  def request1(), do: %{token: "aaa", data: %{a: 42}}

  def request2(), do: %{token: "bbb", data: %{a: 42}}
  
  def request3(), do: %{token: "aaa", data: %{b: 42}}
  
  def request4(), do: %{token: "ccc", data: %{a: 42}}
  
  def request5(), do: %{token: "aaa", data: %{a: 100}}
  
  def handle(request) do
    try do
      authorize(request)
      authenticate(request)
      validate(request)
      result = do_something_useful(request)
      {200, result}
    rescue
      error in [M.AuthenticationError, M.AuthorizationError] ->
        {403, Exception.message(error)}
      error in [M.SchemaValidationError] ->
        {409, Exception.message(error)}
      error ->
        IO.puts(Exception.format(:error, error, __STACKTRACE__))
        {500, "internal server error"}
    end
  end

  def authorize(request) do
    case request.token do
      "aaa" -> :ok
      "bbb" -> :ok
      _ -> raise M.AuthenticationError, {:token, request.token}
    end
  end

  def authenticate(request) do
    case request.token do
      "aaa" -> :ok
      _ -> raise M.AuthorizationError, {:guest, :reconfigure}
    end
  end

  def validate(request) do
    case Map.has_key?(request.data, :a) do
      true -> :ok
      false -> raise M.SchemaValidationError, "some-schema.json"
    end
  end

  def do_something_useful(%{data: %{a: 100}}) do
    raise "something happened"
  end
  
  def do_something_useful(request) do
    request.data.a
  end
  
end

defmodule Model do

  defmodule AuthenticationError do
    defexception [:type, :token, :login]

    @impl true
    def exception({auth_type, data}) do
      case auth_type do
        :token -> %AuthenticationError{type: auth_type, token: data}
        :login -> %AuthenticationError{type: auth_type, login: data}
      end
    end

    @impl true
    def message(error) do
      case error.type do
        :token -> "invalid token"
        :login -> "invalid login"
      end
    end
  end
  
  defmodule AuthorizationError do
    defexception [:role, :action]

    @impl true
    def exception({role, action}) do
      %AuthorizationError{role: role, action: action}
    end

    @impl true
    def message(error) do
      "role '#{error.role}' is not allowed to do action '#{error.action}'"
    end
  end

  defmodule SchemaValidationError do
    defexception [:schema_name]

    @impl true
    def exception(schema_name) do
      %SchemaValidationError{schema_name: schema_name}
    end

    @impl true
    def message(error) do
      "data is not match to schema '#{error.schema_name}'"
    end
  end

end
