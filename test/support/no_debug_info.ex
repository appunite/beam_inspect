defmodule NoDebugInfo do
  @moduledoc false
  @compile debug_info: false

  def foo, do: :bar
end
