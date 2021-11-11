defmodule TauWeb.PageController do
  use TauWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
