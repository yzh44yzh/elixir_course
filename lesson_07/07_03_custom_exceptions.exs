defmodule Lesson_07.Task_03_CustomException do

  def validate(_) do
    raise SchemaValidationError, "my_request.json"
  end

  def validate_and_rescue(request) do
    try do
      validate(request)
    rescue
      error in [SchemaValidationError] ->
        IO.puts("validation failed, schema name is #{error.schema_name}")
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
