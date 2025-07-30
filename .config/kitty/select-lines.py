import re
from kitty.clipboard import set_clipboard_string


def mark(text, args, Mark, extra_cli_args, *a):
    lines = []
    for idx, m in enumerate(re.finditer(r'[^\r\n]+', text)):
        start, end = m.span()
        mark_text = text[start:end].replace('\n', '').replace('\0', '')
        lines.append(mark_text)
        yield Mark(idx, start, end, mark_text, {'idx': idx, 'lines': lines})


def handle_result(args, data, target_window_id, boss, extra_cli_args, *a):
    start_index, end_index = None, None

    for d in data['groupdicts']:
        idx = d['idx']
        if start_index is None:
            start_index, end_index = idx, idx
        elif idx < start_index:
            start_index = idx
        elif idx > end_index:
            end_index = idx

    lines = data['groupdicts'][0]['lines']
    clipboard_string = '\n'.join(lines[start_index:end_index+1])

    set_clipboard_string(clipboard_string)
