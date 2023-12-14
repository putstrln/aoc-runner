ARG HASKELL_VERSION=9.4.7
FROM haskell:$HASKELL_VERSION-slim AS builder
WORKDIR /aoc
COPY stack.yaml package.yaml stack.yaml.lock aoc-runner.cabal /aoc/
RUN stack build --only-dependencies

COPY app /aoc/app
COPY src /aoc/src
RUN stack build --copy-bins --local-bin-path /aoc/bin

FROM haskell:$HASKELL_VERSION-slim
WORKDIR /app
COPY --from=builder /aoc/bin/ /app/
ENTRYPOINT ["./aoc"]
CMD ["./aoc"]
