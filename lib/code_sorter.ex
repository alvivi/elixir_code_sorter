defmodule CodeSorter do
  def sort(string, opts \\ []) do
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    charlist = String.to_charlist(string)

    tokenizer_options = [
      unescape: false,
      # preserve_comments: &preserve_comments/5,
      warn_on_unnecessary_quotes: false
    ]

    parser_options = [
      # literal_encoder: &{:ok, {:__block__, &2, [&1]}}
      # token_metadata: true
    ]

    with {:ok, tokens} <- :elixir.string_to_tokens(charlist, line, file, tokenizer_options),
         {:ok, forms} <- :elixir.tokens_to_quoted(tokens, file, parser_options) do
      forms
      |> put_lookahead_references()
      |> do_sort()
      |> Macro.to_string()
      |> Code.format_string!()
      |> IO.puts()
    end

    :ok
  end

  defp put_lookahead_references({:defmodule, meta, [aliases, [do: contents]]}) do
    {:defmodule, meta, [aliases, [do: put_lookahead_references(contents)]]}
  end

  defp put_lookahead_references({:__block__, meta, contents}) when is_list(contents) do
    {:__block__, meta, put_lookahead_references(contents)}
  end

  defp put_lookahead_references([head | tail]) do
    put_lookahead_references(head, tail)
  end

  defp put_lookahead_references([]) do
    []
  end

  defp put_lookahead_references({:@, attr_meta, [{:doc, doc_meta, doc}]}, tail) do
    closest_definition = find_closest_name(:def, tail)
    updated_doc_meta = Keyword.put(doc_meta, :closest_definition, closest_definition)
    form = {:@, attr_meta, [{:doc, updated_doc_meta, doc}]}

    [form | put_lookahead_references(tail)]
  end

  defp put_lookahead_references({:@, attr_meta, [{:spec, spec_meta, spec}]}, tail) do
    closest_definition = find_closest_name([:def, :defp], tail)
    updated_spec_meta = Keyword.put(spec_meta, :closest_definition, closest_definition)
    form = {:@, attr_meta, [{:spec, updated_spec_meta, spec}]}

    [form | put_lookahead_references(tail)]
  end

  defp put_lookahead_references({:@, attr_meta, [{:typedoc, doc_meta, doc}]}, tail) do
    closest_definition = find_closest_name([:type, :opaque, :typep], tail)
    updated_doc_meta = Keyword.put(doc_meta, :closest_definition, closest_definition)
    form = {:@, attr_meta, [{:typedoc, updated_doc_meta, doc}]}

    [form | put_lookahead_references(tail)]
  end

  defp put_lookahead_references(head, tail) do
    [head | put_lookahead_references(tail)]
  end

  defp find_closest_name(kinds, [{:@, _attr_meta, [{kind, _def_meta, declaration}]} | tail])
       when is_list(kinds) do
    if Enum.member?(kinds, kind) do
      {kind, find_declaration_name(declaration)}
    else
      find_closest_name(kinds, tail)
    end
  end

  defp find_closest_name(kind, [{:@, _attr_meta, [{kind, _def_meta, declaration}]} | _tail]) do
    {kind, find_declaration_name(declaration)}
  end

  defp find_closest_name(kinds, [{kind, _def_meta, definition} | tail])
       when is_list(kinds) do
    if Enum.member?(kinds, kind) do
      {kind, find_definition_name(definition)}
    else
      find_closest_name(kinds, tail)
    end
  end

  defp find_closest_name(kind, [{kind, _def_meta, definition} | _tail]) do
    {kind, find_definition_name(definition)}
  end

  defp find_closest_name(kind, [_head | tail]) do
    find_closest_name(kind, tail)
  end

  defp find_closest_name(_kind, _) do
    nil
  end

  defp find_definition_name([{:when, _meta, [{name, _name_meta, _params} | _guards]} | _tail]) do
    name
  end

  defp find_definition_name([{name, _name_meta, _opts} | _tail]) do
    name
  end

  defp find_definition_name([_head | tail]) do
    find_definition_name(tail)
  end

  defp find_definition_name(_other) do
    nil
  end

  defp find_declaration_name([{:"::", _as_meta, [{name, _name_meta, _} | _decl]} | _tail]) do
    name
  end

  defp find_declaration_name([_head | tail]) do
    find_declaration_name(tail)
  end

  defp find_declaration_name(_other) do
    nil
  end

  defp do_sort({:defmodule, meta, [aliases, [do: contents]]}) do
    {:defmodule, meta, [aliases, [do: do_sort(contents)]]}
  end

  defp do_sort({:__block__, meta, contents}) when is_list(contents) do
    {:__block__, meta, Enum.sort_by(contents, &form_weight/1, &compare_form_weight/2)}
  end

  defp form_weight({:@, _attr_meta, [{:moduledoc, _moduledoc_meta, _docs}]}) do
    :moduledoc
  end

  defp form_weight({:use, _use_meta, [{:__aliases__, _aliases_meta, [name]}, _opts]}) do
    {:use, Atom.to_charlist(name)}
  end

  defp form_weight({:@, _attr_meta, [{:doc, doc_meta, _contents}]}) do
    {:def, name} = Keyword.fetch!(doc_meta, :closest_definition)

    {:doc, {:def, Atom.to_charlist(name)}}
  end

  defp form_weight({:@, _attr_meta, [{:spec, spec_meta, _contents}]}) do
    line = Keyword.fetch!(spec_meta, :line)
    {kind, name} = Keyword.fetch!(spec_meta, :closest_definition)

    {:spec, {kind, Atom.to_charlist(name)}, line}
  end

  defp form_weight({:@, _attr_meta, [{:type, _type_meta, declaration}]}) do
    name = find_declaration_name(declaration)

    {:type, Atom.to_charlist(name)}
  end

  defp form_weight({:@, _attr_meta, [{:opaque, _type_meta, declaration}]}) do
    name = find_declaration_name(declaration)

    {:opaque, Atom.to_charlist(name)}
  end

  defp form_weight({:@, _attr_meta, [{:typedoc, typedoc_meta, _contents}]}) do
    {kind, name} = Keyword.fetch!(typedoc_meta, :closest_definition)

    {:typedoc, {kind, Atom.to_charlist(name)}}
  end

  defp form_weight({:@, _attr_meta, [{name, _name_meta, _name_opts}]}) do
    {:module_attribute, Atom.to_charlist(name)}
  end

  defp form_weight({:defstruct, _meta, _opts}) do
    :defstruct
  end

  defp form_weight({:defimpl, _impl_meta, [{:__aliases__, _name_meta, [name]}, _contents]}) do
    {:defimpl, Atom.to_charlist(name)}
  end

  defp form_weight({:def, meta, definition}) do
    line = Keyword.fetch!(meta, :line)
    name = find_definition_name(definition)

    {:def, Atom.to_charlist(name), line}
  end

  defp form_weight({:defp, def_meta, definition}) do
    line = Keyword.fetch!(def_meta, :line)
    name = find_definition_name(definition)

    {:defp, Atom.to_charlist(name), line}
  end

  defp compare_form_weight(:moduledoc, _rhs), do: true

  # TODO: Behaviour

  defp compare_form_weight({:use, _name}, :moduledoc), do: false
  defp compare_form_weight({:use, lhs}, {:use, rhs}), do: lhs < rhs
  defp compare_form_weight({:use, _name}, _rhs), do: true

  # TODO: Import

  # TODO: Alias

  # TODO: Require

  defp compare_form_weight({:module_attribute, _name}, :moduledoc), do: false
  defp compare_form_weight({:module_attribute, _name}, {:use, _rhs_name}), do: false
  defp compare_form_weight({:module_attribute, lhs}, {:module_attribute, rhs}), do: lhs < rhs
  defp compare_form_weight({:module_attribute, _name}, _rhs), do: true

  defp compare_form_weight(:defstruct, :moduledoc), do: false
  defp compare_form_weight(:defstruct, {:use, _name}), do: false
  defp compare_form_weight(:defstruct, {:module_attribute, _name}), do: false
  defp compare_form_weight(:defstruct, _lhs), do: true

  defp compare_form_weight({:typedoc, {:type, _name}}, :moduledoc), do: false
  defp compare_form_weight({:typedoc, {:type, _name}}, {:use, _rhs_name}), do: false
  defp compare_form_weight({:typedoc, {:type, _name}}, {:module_attribute, _rhs_name}), do: false
  defp compare_form_weight({:typedoc, {:type, _name}}, :defstruct), do: false
  defp compare_form_weight({:typedoc, {:type, lhs}}, {:typedoc, {:type, rhs}}), do: lhs < rhs
  defp compare_form_weight({:typedoc, {:type, name}}, {:type, name}), do: true
  defp compare_form_weight({:typedoc, {:type, lhs}}, {:type, rhs}), do: lhs < rhs
  defp compare_form_weight({:typedoc, {:type, _name}}, _rhs), do: true

  defp compare_form_weight({:type, _name}, :moduledoc), do: false
  defp compare_form_weight({:type, _lhs_name}, {:use, _rhs_name}), do: false
  defp compare_form_weight({:type, _lhs_name}, {:module_attribute, _rhs_name}), do: false
  defp compare_form_weight({:type, _lhs_name}, :defstruct), do: false
  defp compare_form_weight({:type, name}, {:typedoc, {:type, name}}), do: false
  defp compare_form_weight({:type, lhs}, {:typedoc, {:type, rhs}}), do: lhs < rhs
  defp compare_form_weight({:type, lhs}, {:type, rhs}), do: lhs < rhs
  defp compare_form_weight({:type, _name}, _rhs), do: true

  defp compare_form_weight({:typedoc, {:opaque, _name}}, :moduledoc), do: false
  defp compare_form_weight({:typedoc, {:opaque, _name}}, {:use, _rname}), do: false
  defp compare_form_weight({:typedoc, {:opaque, _name}}, {:module_attribute, _rname}), do: false
  defp compare_form_weight({:typedoc, {:opaque, _name}}, :defstruct), do: false
  defp compare_form_weight({:typedoc, {:opaque, _name}}, {:typedoc, {:type, _rname}}), do: false
  defp compare_form_weight({:typedoc, {:opaque, _name}}, {:type, _rname}), do: false
  defp compare_form_weight({:typedoc, {:opaque, lhs}}, {:typedoc, {:opaque, rhs}}), do: lhs < rhs
  defp compare_form_weight({:typedoc, {:opaque, name}}, {:opaque, name}), do: true
  defp compare_form_weight({:typedoc, {:opaque, lhs}}, {:opaque, rhs}), do: lhs < rhs
  defp compare_form_weight({:typedoc, {:opaque, _name}}, _rhs), do: true

  defp compare_form_weight({:opaque, _name}, :moduledoc), do: false
  defp compare_form_weight({:opaque, _name}, {:use, _rname}), do: false
  defp compare_form_weight({:opaque, _name}, {:module_attribute, _rname}), do: false
  defp compare_form_weight({:opaque, _name}, :defstruct), do: false
  defp compare_form_weight({:opaque, _name}, {:typedoc, {:type, _rname}}), do: false
  defp compare_form_weight({:opaque, _name}, {:type, _rname}), do: false
  defp compare_form_weight({:opaque, name}, {:typedoc, {:opaque, name}}), do: false
  defp compare_form_weight({:opaque, lhs}, {:typedoc, {:opaque, rhs}}), do: lhs < rhs
  defp compare_form_weight({:opaque, lhs}, {:opaque, rhs}), do: lhs < rhs
  defp compare_form_weight({:opaque, _name}, _rhs), do: true

  # TODO: Callback

  # TODO: Macro Callback

  # TODO: Optional Callback

  defp compare_form_weight({:defimpl, _name}, :moduledoc), do: false
  defp compare_form_weight({:defimpl, _name}, {:use, _use_name}), do: false
  defp compare_form_weight({:defimpl, _name}, {:module_attribute, _attr_name}), do: false
  defp compare_form_weight({:defimpl, _name}, :defstruct), do: false
  defp compare_form_weight({:defimpl, _name}, {:typedoc, {:type, _rhs_name}}), do: false
  defp compare_form_weight({:defimpl, _name}, {:type, _rhs_name}), do: false
  defp compare_form_weight({:defimpl, _name}, {:typedoc, {:opaque, _rhs_name}}), do: false
  defp compare_form_weight({:defimpl, _name}, {:opaque, _rhs_name}), do: false
  defp compare_form_weight({:defimpl, lhs}, {:defimpl, rhs}), do: lhs > rhs
  defp compare_form_weight({:defimpl, _name}, _rhs), do: true

  defp compare_form_weight({:doc, _ref}, :moduledoc), do: false
  defp compare_form_weight({:doc, _ref}, {:use, _use_name}), do: false
  defp compare_form_weight({:doc, _ref}, {:module_attribute, _attr_name}), do: false
  defp compare_form_weight({:doc, _ref}, :defstruct), do: false
  defp compare_form_weight({:doc, _ref}, {:typedoc, {:type, _rhs_name}}), do: false
  defp compare_form_weight({:doc, _ref}, {:type, _rhs_name}), do: false
  defp compare_form_weight({:doc, _ref}, {:typedoc, {:opaque, _rhs_name}}), do: false
  defp compare_form_weight({:doc, _ref}, {:opaque, _rhs_name}), do: false
  defp compare_form_weight({:doc, _ref}, {:defimpl, _name}), do: false
  defp compare_form_weight({:doc, {:def, lhs}}, {:doc, {:def, rhs}}), do: lhs < rhs
  defp compare_form_weight({:doc, {:def, name}}, {:def, name, _line}), do: true
  defp compare_form_weight({:doc, {:def, lhs}}, {:def, rhs, _line}), do: lhs < rhs
  defp compare_form_weight({:doc, _ref}, _lhs), do: true

  defp compare_form_weight({:spec, {:def, _name}, _line}, :moduledoc), do: false
  defp compare_form_weight({:spec, {:def, _name}, _line}, {:use, _use_name}), do: false
  defp compare_form_weight({:spec, {:def, _name}, _line}, {:module_attribute, _aname}), do: false
  defp compare_form_weight({:spec, {:def, _name}, _line}, :defstruct), do: false
  defp compare_form_weight({:spec, {:def, _name}, _line}, {:typedoc, {:type, _rname}}), do: false
  defp compare_form_weight({:spec, {:def, _name}, _line}, {:type, _rhs_name}), do: false
  defp compare_form_weight({:spec, {:def, _name}, _l}, {:typedoc, {:opaque, _rname}}), do: false
  defp compare_form_weight({:spec, {:def, _name}, _line}, {:opaque, _rhs_name}), do: false
  defp compare_form_weight({:spec, {:def, _lhs_name}, _line}, {:defimpl, _rhs_name}), do: false
  defp compare_form_weight({:spec, {:def, name}, _line}, {:doc, {:def, name}}), do: false
  defp compare_form_weight({:spec, {:def, lhs}, _line}, {:doc, {:def, rhs}}), do: lhs < rhs
  defp compare_form_weight({:spec, {:def, name}, _lhs_line}, {:def, name, _rhs_line}), do: true
  defp compare_form_weight({:spec, {:def, lhs}, _lhs_line}, {:def, rhs, _rhs_line}), do: lhs < rhs
  defp compare_form_weight({:spec, {:def, name}, lhs}, {:spec, {:def, name}, rhs}), do: lhs < rhs
  defp compare_form_weight({:spec, {:def, lhs}, _ll}, {:spec, {:def, rhs}, _rl}), do: lhs < rhs
  defp compare_form_weight({:spec, {:def, _name}, _lhs_line}, _rhs), do: true

  defp compare_form_weight({:def, _name, _line}, :moduledoc), do: false
  defp compare_form_weight({:def, _name, _line}, {:use, _use_name}), do: false
  defp compare_form_weight({:def, _name, _line}, {:module_attribute, _attr_name}), do: false
  defp compare_form_weight({:def, _name, _line}, :defstruct), do: false
  defp compare_form_weight({:def, _name, _line}, {:typedoc, {:type, _rhs_name}}), do: false
  defp compare_form_weight({:def, _name, _line}, {:type, _rhs_name}), do: false
  defp compare_form_weight({:def, _name, _line}, {:typedoc, {:opaque, _rhs_name}}), do: false
  defp compare_form_weight({:def, _name, _line}, {:opaque, _rhs_name}), do: false
  defp compare_form_weight({:def, _lhs_name, _line}, {:defimpl, _rhs_name}), do: false
  defp compare_form_weight({:def, name, _line}, {:doc, {:def, name}}), do: false
  defp compare_form_weight({:def, lhs, _line}, {:doc, {:def, rhs}}), do: lhs < rhs
  defp compare_form_weight({:def, name, lhs}, {:def, name, rhs}), do: lhs < rhs
  defp compare_form_weight({:def, lhs, _lhs_line}, {:def, rhs, _rhs_line}), do: lhs < rhs
  defp compare_form_weight({:def, _name, _line}, _rhs), do: true

  # TODO: Private Type

  defp compare_form_weight({:spec, {:defp, _name}, _line}, :moduledoc), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _line}, {:use, _use_name}), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _line}, {:module_attribute, _aname}), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _line}, :defstruct), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _line}, {:typedoc, {:type, _rname}}), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _line}, {:type, _rhs_name}), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _l}, {:typedoc, {:opaque, _rname}}), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _line}, {:opaque, _rhs_name}), do: false
  defp compare_form_weight({:spec, {:defp, _lhs_name}, _line}, {:defimpl, _rhs_name}), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _line}, {:doc, _ref}), do: false
  defp compare_form_weight({:spec, {:defp, _name}, _lline}, {:def, _rname, _rline}), do: false
  defp compare_form_weight({:spec, {:defp, _lname}, _ll}, {:spec, {:def, _rname}, _rl}), do: false
  defp compare_form_weight({:spec, {:defp, name}, l}, {:spec, {:defp, name}, r}), do: l < r
  defp compare_form_weight({:spec, {:defp, lhs}, _ll}, {:spec, {:defp, rhs}, _rl}), do: lhs < rhs
  defp compare_form_weight({:spec, {:defp, name}, _ll}, {:defp, name, _line}), do: true
  defp compare_form_weight({:spec, {:defp, lhs}, _ll}, {:defp, rhs, _line}), do: lhs < rhs

  defp compare_form_weight({:defp, _name, _line}, :moduledoc), do: false
  defp compare_form_weight({:defp, _name, _line}, {:use, _use_name}), do: false
  defp compare_form_weight({:defp, _name, _line}, {:module_attribute, _attr_name}), do: false
  defp compare_form_weight({:defp, _name, _line}, :defstruct), do: false
  defp compare_form_weight({:defp, _name, _line}, {:typedoc, {:type, _rhs_name}}), do: false
  defp compare_form_weight({:defp, _name, _line}, {:type, _rhs_name}), do: false
  defp compare_form_weight({:defp, _name, _line}, {:typedoc, {:opaque, _rname}}), do: false
  defp compare_form_weight({:defp, _name, _line}, {:opaque, _rhs_name}), do: false
  defp compare_form_weight({:defp, _lhs_name, _line}, {:defimpl, _rhs_name}), do: false
  defp compare_form_weight({:defp, _name, _line}, {:doc, _def_name}), do: false
  defp compare_form_weight({:defp, _name, _ll}, {:spec, {:def, _rname}, _rl}), do: false
  defp compare_form_weight({:defp, _name, _lhs_line}, {:def, _def_name, _rhs_line}), do: false
  defp compare_form_weight({:defp, name, _ll}, {:spec, {:defp, name}, _rl}), do: false
  defp compare_form_weight({:defp, lhs, _ll}, {:spec, {:defp, rhs}, _rl}), do: lhs < rhs
  defp compare_form_weight({:defp, name, lhs}, {:defp, name, rhs}), do: lhs < rhs
  defp compare_form_weight({:defp, lhs, _lhs_line}, {:defp, rhs, _rhs_line}), do: lhs < rhs
end
