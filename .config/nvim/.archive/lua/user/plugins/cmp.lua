return {
  "hrsh7th/nvim-cmp",
  enabled = false,
  dependencies = {
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "hrsh7th/cmp-nvim-lua",
    "hrsh7th/cmp-path",
    "onsails/lspkind.nvim",
    -- vsnip
    "hrsh7th/cmp-vsnip",
    "hrsh7th/vim-vsnip",
  },
  event = "InsertEnter",
  config = function()
    local cmp = require('cmp')
    local lspkind = require('lspkind')
    local CompletionItemKind = cmp.lsp.CompletionItemKind  -- Storing this table, possible for faster access???

    cmp.setup({
      completion = {
        autocomplete = false,
        completeopt = 'menu,menuone,noinsert,noselect,popup',
      },
      formatting = {
        format = function(entry, vim_item)
          vim_item.menu = ''
          return lspkind.cmp_format()(entry, vim_item)
        end,
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<C-y>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<Tab>"] = function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          else
            fallback()
          end
        end,
        ["<S-Tab>"] = function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          else
            fallback()
          end
        end,
      }),
      snippet = {
        expand = function(args)
          vim.fn["vsnip#anonymous"](args.body)
        end,
      },
      sources = cmp.config.sources({
        {
          name = 'nvim_lsp',
          entry_filter = function(entry)
            return CompletionItemKind[entry:get_kind()] ~= "Snippet"
          end
        },
        { name = "nvim_lsp_signature_help" },
      }, {
        { name = 'buffer' },
        { name = "nvim_lua" },
        { name = "path" },
      }),
      window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
      },
    })

    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = 'path' }
      }, {
        { name = 'cmdline' }
      })
    })

    -- debounce (https://github.com/hrsh7th/nvim-cmp/issues/598#issuecomment-984930668)
    local timer = vim.loop.new_timer()
    local DEBOUNCE_DELAY = 500

    function MyCmpDebounce()
      timer:stop()
      timer:start(
        DEBOUNCE_DELAY,
        0,
        vim.schedule_wrap(function()
          cmp.complete({ reason = cmp.ContextReason.Auto })
        end)
      )
    end

    vim.cmd([[
      augroup CmpDebounceAuGroup
        au!
        au TextChangedI * lua MyCmpDebounce()
      augroup end
    ]])

  end,
}
