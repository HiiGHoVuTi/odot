# Odot

```
Odot -- a TODO TUI

Usage: odot (COMMAND | COMMAND)

Available options:
  -h,--help                Show this help text

Available commands:
  tui                      open the tui
  list                     list current todos
  new                      add a new todo
  remove                   removes items
  done                     marks items as done
```

## TUI

[WIP]

```
$ odot tui
```

## List

will list the tasks in a less glamorous format. It can be used to know the index of items to mark them as done or delete them.
```
$ odot list
1- add json exports | from Sun 13/02/2022, 2pm
...
```

You can also sort by tags.
```
$ odot list -t work
3- meeting with Josh | due Mon 14/02/2022, 6pm
```
## New

lets you add tasks
```
Usage: odot new --name NAME [--due DUE]
                (--exp-coef EXP | --lin-coef LIN [--lin-offset OFF
] |
                  --log-coef LOG) (-t|--tags TAGS)
  add a new todo

$ ./**/odot/odot new --name "touch grass" -t life --log-coef 1 --due "13/02/2022 8pm"
```

### due

date formatted like `29/03/2022 2pm`

### tags

a list of tags to add to the item, like `-t "life work"`

### Normal priority

use the linear coefficient like `--lin-coef 1` (1 to 5 advised)

### Growing priority

use the exponential coefficient like `--exp-coef 1` (1-5 advised)

### Background Task

use the logarithmic coefficient like `--log-coef 1` (1-5 advised)

## Remove

removes a list of tasks by indexes

```
$ odot remove -i "1 2 3"
```

## Done

marks a list of tasks as done
```
$ odot done -i "1 2 3"
```
