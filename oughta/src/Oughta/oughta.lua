function check(needle)
  if needle == "" then
    match(0)
    return
  end
  local start, _ = string.find(text, needle, 1, true)
  if start == nil then
    fail()
  else
    seek(start - 1)
    here(needle)
  end
end

function checkln(needle)
  check(needle .. "\n")
end

function here(needle)
  local l = needle:len()
  if string.sub(text, 1, l) == needle then
    match(l)
  else
    fail()
  end
end

function hereln(needle)
  here(needle .. "\n")
end

