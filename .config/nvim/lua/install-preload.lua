local function find_real_loader(modname)
  local searchers = package.searchers or package.loaders
  for _, searcher in ipairs(searchers) do
    local loader, extra = searcher(modname)
    if type(loader) == "function" then
      return loader, extra
    end
  end
  return nil, ("no loader found for %q"):format(modname)
end

return function(modname, on_first_require)
  -- If already loaded, interception is pointless (or too late)
  if package.loaded[modname] then
    return
  end

  -- Don't clobber an existing preload (be conservative)
  if package.preload[modname] then
    error(("package.preload already installed for %s"):format(modname))
  end

  local ran = false

  package.preload[modname] = function()
    -- Disable this hook immediately to avoid re-entry
    package.preload[modname] = nil

    -- Load the real module WITHOUT calling require()
    local loader, extra = find_real_loader(modname)
    if not loader then
      error(extra)
    end

    package.loaded[modname] = true

    local ok, mod = pcall(loader, modname, extra)
    if not ok then
      package.loaded[modname] = nil
      error(mod)
    end

    package.loaded[modname] = mod

    on_first_require(mod)

    return mod
  end
end
