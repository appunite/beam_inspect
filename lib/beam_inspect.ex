defmodule BeamInspect do
  @moduledoc """
  See how your elixir module looks like in erlang/core erlang.

  It requires `:debug_info` or `:abstract_code` in .beam file.
  """

  @doc """
  Returns erlang abstract code

  ## Example

      iex > Foo |> BeamInspect.to_erlang() |> IO.puts()
      :ok

  """
  @spec to_erlang(module()) :: charlist() | no_return()
  def to_erlang(module) when is_atom(module) do
    module
    |> abstract_code()
    |> to_erl()
  end

  @format_opts [:noann]

  @doc """
  Returns core erlang code (erlang abstract code compiled with `+to_core` flag)

  ## Options

    * erlc flags - e.g. +time should be passed as `:time` atom

    * `:noann` - removes compiler annotations

  ## Examples

      iex > Foo |> BeamInspect.to_core_erlang() |> IO.puts()
      :ok

      iex > Foo |> BeamInspect.to_core_erlang(:noann) |> IO.puts()
      :ok

      iex > Foo |> BeamInspect.to_core_erlang(:time) |> IO.puts()
      :ok

      iex > Foo |> BeamInspect.to_core_erlang([:noann, :time]) |> IO.puts()
      :ok

  """
  @spec to_core_erlang(module(), atom() | [atom()]) :: charlist() | no_return()
  def to_core_erlang(module, opts \\ []) when is_atom(module) do
    {format_opts, erlc_flags} = opts |> List.wrap() |> Enum.split_with(&(&1 in @format_opts))

    module
    |> abstract_code()
    |> to_core(erlc_flags, :noann in format_opts)
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

  defp to_core(abstract_code, erlc_flags, noann) do
    {:ok, _, core} = :compile.noenv_forms(abstract_code, [:to_core | erlc_flags])

    :cerl_prettypr.format(core, noann: noann)
  end
end
