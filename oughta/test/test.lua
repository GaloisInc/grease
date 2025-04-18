name = 'Oughta'

-- Assert that there was a match on line `i`.
function match_on_line(i)
  check(string.format('✔️ match at <out>:%d', i))
end

-- Assert that there was a match on the line just before the line of code that
-- calls this function.
function match_prev()
  match_on_line(src_line(1) - 1)
end

-- Assert that there was a match from a function call on line `i`.
function match_from_line(i)
  checkln 'stack trace:'
  here(string.format('  %s:%d', file(), i))
end
