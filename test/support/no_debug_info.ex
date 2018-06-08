defmodule NoDebugInfo do
  @compile debug_info: false

  def foo, do: :bar
end
