c.url.searchengines['yt'] = 'https://www.youtube.com/results?search_query={}'
c.url.searchengines['br'] = 'https://search.brave.com/search?q={}&source=web'

c.colors.statusbar.private.bg = '#330033'

#c.colors.webpage.darkmode.enabled = True
c.content.autoplay = False

config.bind('M', 'hint links spawn mpv {hint-url}')
# config.bind('M', 'hint links spawn streamlink --default-stream "720p,480p,best" {hint-url}')
config.bind('Z', 'hint links spawn st -e youtube-dl {hint-url}')

config.bind('<Ctrl-j>', 'completion-item-focus next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus prev', mode='command')
config.bind('<Ctrl-Shift-n>', 'set-cmd-text -s :open -p')
config.bind('<Ctrl-t>', 'open -t;; set-cmd-text -s :open')
config.bind('<Ctrl-r>', 'reload')

config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')

config.load_autoconfig()
