defmodule BeamInspect do
  @moduledoc """
  Inspect how your elixir module looks like in erlang / core erlang.
  """

  @doc """
  Returns erlang code.

  Abstract erlang code is fetched from .beam file.
  It requires `:debug_info` or `:abstract_code` to be available in compiled module.

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
  Returns core erlang code.

  Abstract erlang code is fetched from .beam file.
  It requires `:debug_info` or `:abstract_code` to be available in compiled module.
  Erlang abstract code is compiled with `+to_core` flag by `:compile.noenv_forms/2` function.

  ## Options

    * erlc flags - e.g. erlc +time should be passed as `:time` atom

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
    {format_opts, erlc_flags} = opts |> List.wrap() |> split_opts(&(&1 in @format_opts))

    module
    |> abstract_code()
    |> to_core(erlc_flags, :noann in format_opts)
  end

  defp abstract_code(module) do
    file = :code.which(module)

    case :beam_lib.chunks(file, [:debug_info]) do
      {:ok, {^module, [{:debug_info, {:debug_info_v1, backend, {_, _, _} = metadata}}]}} ->
        {:ok, abstract_code} = backend.debug_info(:erlang_v1, module, metadata, [])
        abstract_code

      _ ->
        case :beam_lib.chunks(file, [:abstract_code]) do
          {:ok, {^module, [{:abstract_code, {:raw_abstract_v1, abstract_code}}]}} ->
            abstract_code

          _ ->
            raise "abstract code unavailable"
        end
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

  @compile {:inline, split_opts: 2}
  if Version.match?(System.version(), "< 1.4.0") do
    defp split_opts(opts, fun), do: Enum.partition(opts, fun)
  else
    defp split_opts(opts, fun), do: Enum.split_with(opts, fun)
  end
end
