return {
  "williamboman/mason.nvim",
  -- event: Per documentation, don't make lazy.
  build = ":MasonUpdate", -- :MasonUpdate updates registry contents
  config = true,
}
