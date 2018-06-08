defmodule BeamInspectTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  describe "to_erlang/1" do
    test "with debug info present" do
      assert BeamInspect.to_erlang(GenServer) |> to_string() =~ "-module('Elixir.GenServer')"
    end

    test "without debug info present" do
      assert_raise(RuntimeError, "abstract code unavailable", fn ->
        BeamInspect.to_erlang(NoDebugInfo)
      end)
    end
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

    test "without debug info present" do
      assert_raise(RuntimeError, "abstract code unavailable", fn ->
        BeamInspect.to_core_erlang(NoDebugInfo)
      end)
    end
  end
end
