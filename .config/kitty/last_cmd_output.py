from typing import List
from kitty.boss import Boss
import os
import shlex
import sys
import tempfile

# in main, STDIN is for the kitten process and will contain
# the contents of the screen
def main(args: List[str]) -> str:
    return sys.stdin.read()

# in handle_result, STDIN is for the kitty process itself, rather
# than the kitten process and should not be read from.
from kittens.tui.handler import result_handler
@result_handler(type_of_input='output')
def handle_result(args: List[str], stdin_data: str, target_window_id: int, boss: Boss) -> None:
    w = boss.window_id_map.get(target_window_id)
    if w is not None:
        command = f'echo {shlex.quote(stdin_data)} | vim -u "$HOME/.config/kitty/scrollback_pager.vim" -'
        boss.call_remote_control(w, ('launch', '--type=overlay', '--no-response', 'sh', '-c', command))
