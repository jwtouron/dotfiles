#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_ITEMS 10
#define SQUARE(x) ((x) * (x))

// Single-line comment
/*
 * Multi-line comment.
 * TODO: this may also trigger special comment highlighting.
 */

typedef struct Person {
    const char *name;
    uint32_t age;
    bool active;
} Person;

enum Status {
    STATUS_OK = 0,
    STATUS_WARNING,
    STATUS_ERROR,
};

static const double PI = 3.141592653589793;

static int add(int a, int b)
{
    return a + b;
}

static void print_person(const Person *person)
{
    if (person == NULL) {
        fprintf(stderr, "person is NULL\n");
        return;
    }

    printf(
        "name=\"%s\", age=%u, active=%s\n",
        person->name,
        person->age,
        person->active ? "true" : "false"
    );
}

int main(int argc, char **argv)
{
    Person people[MAX_ITEMS] = {
        {
            .name = "Alice",
            .age = 27,
            .active = true,
        },
        {
            .name = "Bob\tSmith",
            .age = 42,
            .active = false,
        },
    };

    char letter = 'A';
    unsigned int hex = 0xDEADBEEF;
    int binary_like = 0b101010; /* C23 / compiler extension */
    double value = PI * 2.0;
    int result = add(3, 4);

    printf("letter: %c\n", letter);
    printf("hex: %#x\n", hex);
    printf("value: %.2f\n", value);
    printf("square: %d\n", SQUARE(result));

    for (size_t i = 0; i < 2; ++i) {
        print_person(&people[i]);
    }

    while (result > 0) {
        result--;

        if (result == 2)
            continue;

        if (result == 1)
            break;
    }

    switch (argc) {
    case 0:
        goto done;

    case 1:
        puts("No arguments");
        break;

    default:
        printf("First argument: %s\n", argv[1]);
        break;
    }

done:
    return EXIT_SUCCESS;
}
