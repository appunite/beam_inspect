defmodule BeamInspect do
  def to_erlang(module) when is_atom(module) do
    module
    |> abstract_code()
    |> to_erl()
  end

  def to_core_erlang(module, erlc_flags \\ []) when is_atom(module) do
    module
    |> abstract_code()
    |> to_core(erlc_flags)
  end

  defp abstract_code(module) do
    file = :code.which(module)

    with {:error, :beam_lib, _} <- :beam_lib.chunks(file, [:debug_info]),
         {:error, :beam_lib, _} <- :beam_lib.chunks(file, [:abstract_code]) do
      raise "Abstract code unavailable"
    else
      {:ok, {^module, [{:debug_info, {:debug_info_v1, backend, metadata}}]}} ->
        {:ok, abstract_code} = backend.debug_info(:erlang_v1, module, metadata, [])
        abstract_code

      {:ok, {^module, [{:abstract_code, {:raw_abstract_v1, abstract_code}}]}} ->
        abstract_code
    end
  end

  defp to_erl(abstract_code) do
    abstract_code
    |> :erl_syntax.form_list()
    |> :erl_prettypr.format()
  end

  defp to_core(abstract_code, erlc_flags) do
    {:ok, _, core} = :compile.noenv_forms(abstract_code, [:to_core | erlc_flags])

    :cerl_prettypr.format(core)
  end
end
