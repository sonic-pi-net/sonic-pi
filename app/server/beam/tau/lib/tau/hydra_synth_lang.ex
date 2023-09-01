defmodule Tau.HydraSynthLang do
  @moduledoc false
  @hydra_global_fns [
    "noise",
    "voronoi",
    "osc",
    "shape",
    "gradient",
    "src",
    "solid",
    "prev",
    "render",
    "update",
    "setResolution",
    "hush",
    "setFunction"
  ]

  @hydra_global_attrs [
    "a",
    "bpm",
    "time",
    "mouse",
    "speed",
    "width",
    "height",
    "o0",
    "o1",
    "o2",
    "o3",
    "s0",
    "s1",
    "s2",
    "s3"
  ]

  @hydra_global_fns_regexp Enum.map(@hydra_global_fns, fn el ->
                             {el, Regex.compile!("\\b#{el}[[:space:]]*\\(")}
                           end)
  @hydra_global_attrs_regexp Enum.map(@hydra_global_attrs, fn el ->
                               {el, Regex.compile!("\\b#{el}\\b")}
                             end)

  def convert_global_syntax_to_instance(code_str) do
    code_str =
      Enum.reduce(@hydra_global_fns_regexp, code_str, fn {el, regexp}, res ->
        String.replace(res, regexp, "hydra.#{el}(")
      end)

    code_str =
      String.replace(
        code_str,
        ~r"\([[:space:]]*{[[:space:]]*time[[:space:]]*}[[:space:]]*\)",
        "({___time___})"
      )

    Enum.reduce(@hydra_global_attrs_regexp, code_str, fn {el, regexp}, res ->
      String.replace(res, regexp, "hydra.#{el}")
    end)
  end

  def eval_hydra(code) do
    instance_code = convert_global_syntax_to_instance(code)
    TauWeb.Endpoint.broadcast("room:hydra", "msg", {instance_code})
    :ok
  end
end
