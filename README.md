# aoc-runner
A small cli app to download yearly [advent of code](https://adventofcode.com/) problems and submit answers.

## Building
```docker build -t aoc .``` It takes a few mins.

## Running
```docker run -v $PWD/data:/app/data aoc COMMAND```

For example,
```docker run -v $PWD/data:/app/data aoc session SESSION_COOKIE_VALUE``` to set the user session.

```docker run -v $PWD/data:/app/data aoc dl``` will download today's problem description and input file into your host's /tmp/aoc folder.

```docker run -v $PWD/data:/app/data aoc ans YOUR_ANSWER``` will submit YOUR_ANSWER for today.

For specifying extra params such as day, check
```docker run -v $PWD/data:/app/data aoc --help```

## Development
You can either build and use the container to develop locally or use stack.

e.g. `stack build && stack run dl 1`
