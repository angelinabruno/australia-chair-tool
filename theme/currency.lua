-- Currency toggle: for every chart image <name>.png that has a US$ twin
-- <name>_usd.png (written by save_both() in '0. Exchange rates.R'), store the
-- twin in the image's data-usd attribute as an embedded data URI. The script
-- in theme/navigation.html swaps the two when the A$ / US$ toggle is used.

local function file_exists(path)
  local f = io.open(path, "rb")
  if f then f:close() return true end
  return false
end

local function read_all(path)
  local f = io.open(path, "rb")
  local s = f:read("a")
  f:close()
  return s
end

function Image(img)
  local src = img.src:gsub("%%20", " ")
  if not src:match("%.png$") or src:match("_usd%.png$") then return nil end
  local twin = src:gsub("%.png$", "_usd.png")
  if not file_exists(twin) then return nil end
  img.attributes["data-usd"] = "data:image/png;base64," .. quarto.base64.encode(read_all(twin))
  return img
end
