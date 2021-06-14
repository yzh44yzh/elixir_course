defmodule Lesson_07.Task_03_CustomException do

  def handle(request) do
    try do
      validate(request)
      do_action(request)
    rescue
      error in [SchemaValidationError] ->
        IO.puts("validation failed, schema name is #{error.schema_name}")
        :error
      error in [AuthorizationError] ->
        IO.puts("authorization failed, #{error.message}")
        :error
    end
  end

  def validate(request) do
    case request do
      1 -> raise SchemaValidationError, "my_request.json"
      _ -> :ok
    end
  end

  def do_action(request) do
    case request do
      2 -> raise AuthorizationError, {"guest", "modify"}
      _ -> :ok
    end
  end

end


defmodule SchemaValidationError do
  
    defexception [
      :message,
      :schema_name
    ]

    @impl true
    def exception(schema_name) do
      msg = "object doesn't match schema \"#{schema_name}\""
      %SchemaValidationError{message: msg, schema_name: schema_name}
    end
    
end


defmodule AuthorizationError do

  defexception [
    :message,
    :role,
    :action
  ]

  @impl true
  def exception({role, action}) do
    msg = "user with role \"#{role}\" doesn't have permission to do action \"#{action}\""
    %AuthorizationError{message: msg, role: role, action: action}
  end

end
