defmodule BeamInspectTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test "to_erlang/1" do
    assert BeamInspect.to_erlang(GenServer) |> to_string() =~ "-module('Elixir.GenServer')"
  end

  describe "to_core_erlang/2" do
    test "with empty opts list" do
      assert BeamInspect.to_core_erlang(GenServer, []) |> to_string() =~
               "module 'Elixir.GenServer'\n"
    end

    test "with +time option" do
      output = capture_io(fn -> BeamInspect.to_core_erlang(GenServer, :time) end)

      assert output =~ "sys_core_fold"
    end

    test "with :noann option" do
      refute BeamInspect.to_core_erlang(GenServer, :noann) |> to_string() =~
               "-| [{'file', \"lib/gen_server.ex\"}]"
    end

    test "with multiple options" do
      assert capture_io(fn ->
               refute BeamInspect.to_core_erlang(GenServer, [:noann, :time]) |> to_string() =~
                        "-| [{'file', \"lib/gen_server.ex\"}]"
             end) =~ "sys_core_fold"
    end
  end
end
