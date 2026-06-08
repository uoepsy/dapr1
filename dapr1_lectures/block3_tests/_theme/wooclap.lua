function Div(el)
  -- Check if the div has class "woo"
  if el.classes:includes("woo") then
    -- Get the plain text inside the :::woo ... :::
    local link = pandoc.utils.stringify(el.content)

    -- Build the button HTML
    local html = string.format(
      '<button class="iframe-trigger" data-src="%s">woo!</button>',
      link
    )

    -- Return raw HTML so Pandoc doesn't escape it
    return pandoc.RawBlock("html", html)
  end
end