-- One-line takeaways: for every chart image listed in takeaways.tsv (written
-- by takeaway() in '0. Takeaways.R' when the chart scripts run), add the line
-- under the chart. Both currency versions are kept in data-aud-text /
-- data-usd-text so the A$ / US$ toggle in theme/navigation.html swaps them.

local lines = {}

local function load()
  local f = io.open("takeaways.tsv", "r")
  if not f then return end
  local header = true
  for row in f:lines() do
    row = row:gsub("\r$", "")
    if header then
      header = false
    else
      local chart, aud, usd = row:match("^([^\t]*)\t([^\t]*)\t?([^\t]*)$")
      if chart and aud ~= "" then
        lines[chart] = { aud = aud, usd = (usd ~= "" and usd) or aud }
      end
    end
  end
  f:close()
end
load()

local function esc(s)
  return (s:gsub("&", "&amp;"):gsub('"', "&quot;"):gsub("<", "&lt;"):gsub(">", "&gt;"))
end

function Para(p)
  if #p.content ~= 1 or p.content[1].t ~= "Image" then return nil end
  local src = p.content[1].src:gsub("%%20", " ")
  local t = lines[src]
  if not t then return nil end
  local html = string.format(
    '<p class="chart-takeaway" data-aud-text="%s" data-usd-text="%s">%s</p>',
    esc(t.aud), esc(t.usd), esc(t.aud))
  return { p, pandoc.RawBlock("html", html) }
end
