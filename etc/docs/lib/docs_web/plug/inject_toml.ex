require IEx

defmodule DocsWeb.Plug.InjectToml do
  import Plug.Conn

  defp file_list(dir) do
    toml = Path.wildcard("#{:code.priv_dir(:docs)}/toml/#{dir}/*.toml")
    Enum.map(toml, fn file ->
      {:ok, data} = Toml.decode_file(file, keys: :atoms)
      data
    end)
  end


  defp directory_list do
    dirs = File.cd!(
      "#{:code.priv_dir(:docs)}/toml",
      fn -> File.ls! |> Enum.filter(&File.dir?(Path.join("#{:code.priv_dir(:docs)}/toml", &1))) end
    )
    for dir <- dirs, into: %{}, do: {String.to_atom(dir), file_list(dir)}
  end

  def metadata(f) do
    key = make_ref()
    fn ->
      case :ets.lookup(:session, key) do
        [{^key, val}] -> val
        [] ->
          val = f.()
          :ets.insert(:session, {key, val})
          val
      end
    end
  end

  def init(default), do: default

  def call(conn, _default) do
      conn
      |> put_session(:metadata, metadata(&directory_list/0).())
  end
end
