#define _GNU_SOURCE
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char *skip_args(const char * const escape_code)
{
    const char *s = escape_code;
    while (*s != 0) {
        if ((*s >= '0' && *s <= '9') || *s == ';' || *s == ':') {
            s++;
        } else {
            return s;
        }
    }
    return s;
}

static const char *skip_close_bracket(const char * const escape_code)
{
    if (strncmp(escape_code + 2, "8;;", 3) == 0) {
        return &escape_code[5];
    } else {
        const char *s = skip_args(escape_code + 2);
        switch (*s) {
            case 'A':
            case 'C':
                return s + 1;
            default:
                return escape_code;
        }
    }
}

static const char *skip_open_bracket(const char * const escape_code)
{
    const char *s = skip_args(escape_code + 2);
    switch (*s) {
        case 'm':
            return s + 1;
        default:
            return escape_code;
    }
}

static const char *skip_escape_code(const char * const escape_code)
{
    const char *s = escape_code;
    switch (*++s) {
        case '\\':
            return ++s;
        case '[':
            return skip_open_bracket(escape_code);
        case ']':
            return skip_close_bracket(escape_code);
        default:
            return escape_code;
    }
}

static void flush_stdin()
{
    int c;
    while ((c = fgetc(c, stdin)) != EOF) {
        fputc(c, stdout);
    }
}

int main()
{
    size_t n = 1024;
    const char * const lineptr = malloc(n);
    if (lineptr == NULL) {
        flush_stdin();
        return 0;
    }

    ssize_t result = getline((char **)&lineptr, &n, stdin);
    while (result > 0) {
        const char *s = lineptr;
        while (*s != 0) {
            if (*s == '\x1b') {
                const char *s0 = skip_escape_code(s);
                if (s0 != s) {
                    s = s0;
                    continue;
                }
            } 
            fputc(*s++, stdout);
        }
        result = getline((char **)&lineptr, &n, stdin);
    }

    if (result == -1 && errno == ENOMEM) {
        fprintf(stderr, "No memory (n): %zu\n", n);
        flush_stdin();
    }

    return 0;
}
