local M = {}

M.setup = function()
  for f in pairs(M) do
    if f:match("^[A-Z]") then
      _G[f] = M[f]
    end
  end
end

return M
