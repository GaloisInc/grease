function ok() check 'All goals passed' end

function exception() check 'Exception: ' end

function user_error(msg)
  check 'User error: '
  check(msg)
end

function could_not_infer() check 'Possible bug(s)' end

function must_fail() check 'Likely bug: unavoidable error' end

function next_line_must_fail()
  must_fail()
  check(string.format('%s:%d', file(), src_line(1) + 1))
end

function no_heuristic() check 'Unable to find a heuristic for any goal' end

function req_failed() check 'One or more assertions failed' end

function uninit_stack() check 'Likely bug: uninitialized stack read' end
