require IEx

defmodule DocsWeb.PageView do
  use DocsWeb, :view

  def is_active(tab, which_tab) do
    if tab != nil && tab == which_tab do
      "nav-link active"
    else
      "nav-link"
    end
  end

  # def content_component(assigns) do
  #   case assigns.active_tab do
  #     x when x == "synths" or x == "fx" -> DocsWeb.PageLive.SynthsOrFxComponent
  #     "samples" -> DocsWeb.PageLive.SamplesComponent
  #     "lang" -> DocsWeb.PageLive.LangComponent
  #     nil -> DocsWeb.PageLive.WelcomeComponent
  #   end
  # end

  def test do
    "hello"
  end
end
