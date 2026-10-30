-- Quotable charts: for every chart image <name>.png, attach the data behind it
-- so the footer script in theme/chart-footer.html can offer a CSV download and
-- say how recent the data are.
--   data-csv     the chart's CSV as an embedded data URI, so the link works
--                wherever the dashboard file is opened. <name>_data.csv
--                (written by '0. Chart data.R') is preferred, then <name>.csv.
--   data-csv-name  file name for the download
--   data-period  the latest period in the CSV that is not in the future
--                ("Data to"), e.g. "June 2026", "2025" or "2025-26".

local function read_all(path)
  local f = io.open(path, "rb")
  if not f then return nil end
  local s = f:read("a")
  f:close()
  return s
end

local MAX_CSV = 3 * 1024 * 1024   -- larger files are not embedded

local function url_encode(s)
  return (s:gsub("[^%w%-%._~]", function(c) return string.format("%%%02X", string.byte(c)) end))
end

-- Split one CSV line into fields (handles quoted fields with commas)
local function split_csv(line)
  local out, field, i, inq = {}, {}, 1, false
  line = line:gsub("\r$", "")
  while i <= #line do
    local c = line:sub(i, i)
    if inq then
      if c == '"' then
        if line:sub(i + 1, i + 1) == '"' then field[#field + 1] = '"'; i = i + 1 else inq = false end
      else field[#field + 1] = c end
    elseif c == '"' then inq = true
    elseif c == "," then out[#out + 1] = table.concat(field); field = {}
    else field[#field + 1] = c end
    i = i + 1
  end
  out[#out + 1] = table.concat(field)
  return out
end

local PERIOD_COLS = { date = true, year = true, period = true, month = true, quarter = true,
                      fy = true, ref_fy = true, financial_year = true, filing_year = true,
                      time_period = true }
local MONTHS = { "January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December" }
local TODAY = os.date("%Y-%m-%d")
local THIS_YEAR = tonumber(os.date("%Y"))

-- Comparable key and label for a period value; nil if not a period or in the future
local function period_of(v)
  v = (v or ""):gsub("^%s+", ""):gsub("%s+$", "")
  local y, m, d = v:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)$")
  if y then
    if v > TODAY then return nil end
    return v, MONTHS[tonumber(m)] .. " " .. y
  end
  local fy1, fy2 = v:match("^(%d%d%d%d)%-(%d%d)$")
  if fy1 then
    if tonumber(fy1) > THIS_YEAR then return nil end
    return fy1 .. "-07-01", v
  end
  local q, qy = v:match("^Q([1-4]) (%d%d%d%d)$")
  if q then
    if tonumber(qy) > THIS_YEAR then return nil end
    return qy .. "-" .. string.format("%02d", tonumber(q) * 3) .. "-00", v
  end
  local yr = v:match("^(%d%d%d%d)$") or v:match("^(%d%d%d%d)%.0$")
  if yr then
    if tonumber(yr) > THIS_YEAR then return nil end
    return yr .. "-00-00", yr
  end
  return nil
end

local function latest_period(text)
  local lines = {}
  for l in text:gmatch("[^\n]+") do lines[#lines + 1] = l end
  local best_key, best_lab, months, n_dates
  local function reset() best_key, best_lab, months, n_dates = nil, nil, {}, 0 end
  local function consider(v)
    local k, lab = period_of(v)
    if not k then return end
    local m = (v or ""):match("^%s*%d%d%d%d%-(%d%d)%-%d%d")
    if m then months[tonumber(m)] = true; n_dates = n_dates + 1 end
    if not best_key or k > best_key then best_key, best_lab = k, lab end
  end
  -- Dates that are all quarter starts (Jan, Apr, Jul, Oct) or all quarter ends
  -- are quarterly: say "September quarter 2026", not "July 2026"
  local function label()
    if not best_key or n_dates < 2 then return best_lab end
    local y, m = best_key:match("^(%d%d%d%d)%-(%d%d)%-%d%d$")
    if not y then return best_lab end
    local starts, ends = true, true
    for mm in pairs(months) do
      if mm % 3 ~= 1 then starts = false end
      if mm % 3 ~= 0 then ends = false end
    end
    local MONTHS_ = { "March", "June", "September", "December" }
    if starts then return MONTHS_[(tonumber(m) - 1) // 3 + 1] .. " quarter " .. y end
    if ends then return MONTHS_[tonumber(m) // 3] .. " quarter " .. y end
    return best_lab
  end
  -- A header with a period-like column name; if its rows give no period, try the next
  for h = 1, #lines do
    local f = split_csv(lines[h])
    local cols = {}
    for j, name in ipairs(f) do
      if PERIOD_COLS[(name:lower():gsub("^%s+", ""):gsub("%s+$", ""))] then cols[#cols + 1] = j end
    end
    if #cols > 0 then
      reset()
      for r = h + 1, #lines do
        local row = split_csv(lines[r])
        for _, j in ipairs(cols) do consider(row[j]) end
      end
      if best_key then return label() end
    end
  end
  -- Otherwise rows that start with a period (the export_chart_csv layout)
  reset()
  for _, l in ipairs(lines) do consider(split_csv(l)[1]) end
  return label()
end

function Image(img)
  local src = img.src:gsub("%%20", " ")
  if not src:match("%.png$") or src:match("_usd%.png$") then return nil end
  local stem = src:gsub("%.png$", "")
  local csv_path, csv_text, period
  for _, cand in ipairs({ stem .. "_data.csv", stem .. ".csv" }) do
    local text = read_all(cand)
    if text then
      if not csv_text then csv_path, csv_text = cand, text end
      period = period or latest_period(text)
    end
  end
  if not csv_text then return nil end
  if period then img.attributes["data-period"] = period end
  if #csv_text <= MAX_CSV then
    img.attributes["data-csv"] = "data:text/csv;charset=utf-8," .. url_encode(csv_text)
    img.attributes["data-csv-name"] = csv_path:match("([^/]+)$")
  end
  return img
end
