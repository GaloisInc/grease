-- Screach test prelude

function reached(f)
  check(string.format("Reached target function '%s'", f))
  checkln '(sat)'
  check 'Reached target!'
end

function verified()
  check_not 'Failed to verify reachability!'
  check 'Verified reachability'
end

function failed_to_reach() check 'Failed to reach target' end

function could_not_resolve_target_address(f)
  check(string.format("Could not resolve an address for the target function '%s'", f))
end

function execute_insn_string(addr)
  return string.format("Executing instruction at 0x%x", addr)
end


function not_until(needle, target)
  local start, _ = string.find(text, needle, 1, true)
  if start == nil then
    fail()
  else
    local start_of_not, end_of_not = string.find(text, target, start-1, true)
    if start_of_not ~= nil and end_of_not < start then 
      fail()
    end 
    seek(start - 1)
    here(needle)
  end
end
