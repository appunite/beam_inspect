# BeamInspect

See how your elixir module looks like in erlang/core erlang.

## Installation

The package can be installed by adding it to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:beam_inspect, github: "amatalai/beam_inspect", only: [:dev], runtime: false}
  ]
end
```

## Usage

#### Example elixir module:

```elixir
defmodule Foo do
  def baz do
    :a == :b
  end
end
```

#### See erlang abstract code

```elixir
iex(1)> Foo |> BeamInspect.to_erlang() |> IO.puts()
# -file("lib/foo.ex", 1).
#
# -module('Elixir.Foo').
#
# -compile(no_auto_import).
#
# -export(['__info__'/1, baz/0]).
#
# -spec '__info__'(attributes | compile | functions |
#                  macros | md5 | module | deprecated) -> atom() |
#                                                         [{atom(), any()} |
#                                                          {atom(), byte(),
#                                                           integer()}].
#
# '__info__'(module) -> 'Elixir.Foo';
# '__info__'(functions) -> [{baz, 0}];
# '__info__'(macros) -> [];
# '__info__'(attributes) ->
#     erlang:get_module_info('Elixir.Foo', attributes);
# '__info__'(compile) ->
#     erlang:get_module_info('Elixir.Foo', compile);
# '__info__'(md5) ->
#     erlang:get_module_info('Elixir.Foo', md5);
# '__info__'(deprecated) -> [].
#
# baz() -> a == b.
:ok
```

#### See erlang abstract code compiled with `to_core` flag

```elixir
iex(1)> Foo |> BeamInspect.to_core_erlang() |> IO.puts()
# module 'Elixir.Foo' ['__info__'/1, 'baz'/0,
#                      'module_info'/0, 'module_info'/1]
#   attributes [% Line 1
#               'file' =
#                   % Line 1
#                   [{"lib/foo.ex", 1}],
#               % Line 1
#               'compile' =
#                   % Line 1
#                   ['no_auto_import'],
#               % Line 1
#               'spec' =
#                   % Line 1
#                   [{{'__info__', 1},
#                     [{'type', 1, 'fun',
#                       [{'type', 1, 'product',
#                         [{'type', 1, 'union',
#                           [{'atom', 1, 'attributes'},
#                            {'atom', 1, 'compile'},
#                            {'atom', 1, 'functions'},
#                            {'atom', 1, 'macros'}, {'atom', 1, 'md5'},
#                            {'atom', 1, 'module'},
#                            {'atom', 1, 'deprecated'}]}]},
#                        {'type', 1, 'union',
#                         [{'type', 1, 'atom', []},
#                          {'type', 1, 'list',
#                           [{'type', 1, 'union',
#                             [{'type', 1, 'tuple',
#                               [{'type', 1, 'atom', []},
#                                {'type', 1, 'any', []}]},
#                              {'type', 1, 'tuple',
#                               [{'type', 1, 'atom', []},
#                                {'type', 1, 'byte', []},
#                                {'type', 1, 'integer', []}]}]}]}]}]}]}]]
#   '__info__'/1 =
#       (fun ((_@c0 -| [{'file', "lib/foo.ex"}])) ->
#            (case (_@c0 -| [{'file', "lib/foo.ex"}]) of
#               (('module' -| [{'file', "lib/foo.ex"}])
#                  when 'true' ->
#                    ('Elixir.Foo' -| [{'file', "lib/foo.ex"}])
#                 -| [{'file', "lib/foo.ex"}])
#               (('functions' -| [{'file', "lib/foo.ex"}])
#                  when 'true' ->
#                    ([{'baz', 0}] -| [{'file', "lib/foo.ex"}])
#                 -| [{'file', "lib/foo.ex"}])
#               (('macros' -| [{'file', "lib/foo.ex"}])
#                  when 'true' ->
#                    ([] -| [{'file', "lib/foo.ex"}])
#                 -| [{'file', "lib/foo.ex"}])
#               (('attributes' -| [{'file', "lib/foo.ex"}])
#                  when 'true' ->
#                    (call ('erlang' -| [{'file',
#                                         "lib/foo.ex"}]):('get_module_info'
#                                                           -| [{'file',
#                                                                "lib/foo.ex"}])(('Elixir.Foo'
#                                                                                  -| [{'file',
#                                                                                       "lib/foo.ex"}]),
#                                                                                ('attributes'
#                                                                                  -| [{'file',
#                                                                                       "lib/foo.ex"}]))
#                      -| [{'file', "lib/foo.ex"}])
#                 -| [{'file', "lib/foo.ex"}])
#               (('compile' -| [{'file', "lib/foo.ex"}])
#                  when 'true' ->
#                    (call ('erlang' -| [{'file',
#                                         "lib/foo.ex"}]):('get_module_info'
#                                                           -| [{'file',
#                                                                "lib/foo.ex"}])(('Elixir.Foo'
#                                                                                  -| [{'file',
#                                                                                       "lib/foo.ex"}]),
#                                                                                ('compile'
#                                                                                  -| [{'file',
#                                                                                       "lib/foo.ex"}]))
#                      -| [{'file', "lib/foo.ex"}])
#                 -| [{'file', "lib/foo.ex"}])
#               (('md5' -| [{'file', "lib/foo.ex"}])
#                  when 'true' ->
#                    (call ('erlang' -| [{'file',
#                                         "lib/foo.ex"}]):('get_module_info'
#                                                           -| [{'file',
#                                                                "lib/foo.ex"}])(('Elixir.Foo'
#                                                                                  -| [{'file',
#                                                                                       "lib/foo.ex"}]),
#                                                                                ('md5'
#                                                                                  -| [{'file',
#                                                                                       "lib/foo.ex"}]))
#                      -| [{'file', "lib/foo.ex"}])
#                 -| [{'file', "lib/foo.ex"}])
#               (('deprecated' -| [{'file', "lib/foo.ex"}])
#                  when 'true' ->
#                    ([] -| [{'file', "lib/foo.ex"}])
#                 -| [{'file', "lib/foo.ex"}])
#               (_@c1 when 'true' ->
#                    (primop 'match_fail'(({'function_clause',
#                                           _@c1}
#                                           -| [{'file', "lib/foo.ex"}]))
#                      -| [{'function_name', {'__info__', 1}},
#                          {'file', "lib/foo.ex"}])
#                 -| ['compiler_generated'])
#             end
#              -| [{'file', "lib/foo.ex"}])
#         -| [{'file', "lib/foo.ex"}])
#   'baz'/0 =
#       % Line 2
#       (fun () ->
#            % Line 3
#            ('false' -| [{'file', "lib/foo.ex"}])
#         -| [{'file', "lib/foo.ex"}])
#   'module_info'/0 =
#       (fun () ->
#            (call ('erlang' -| [{'file',
#                                 "lib/foo.ex"}]):('get_module_info'
#                                                   -| [{'file',
#                                                        "lib/foo.ex"}])(('Elixir.Foo'
#                                                                          -| [{'file',
#                                                                               "lib/foo.ex"}]))
#              -| [{'file', "lib/foo.ex"}])
#         -| [{'file', "lib/foo.ex"}])
#   'module_info'/1 =
#       (fun ((_@c0 -| [{'file', "lib/foo.ex"}])) ->
#            (call ('erlang' -| [{'file',
#                                 "lib/foo.ex"}]):('get_module_info'
#                                                   -| [{'file',
#                                                        "lib/foo.ex"}])(('Elixir.Foo'
#                                                                          -| [{'file',
#                                                                               "lib/foo.ex"}]),
#                                                                        (_@c0
#                                                                          -| [{'file',
#                                                                               "lib/foo.ex"}]))
#              -| [{'file', "lib/foo.ex"}])
#         -| [{'file', "lib/foo.ex"}])
# end
:ok
```
