import Distribution.Simple

-- |Change preconfigure hooks - build Halide objects, etc.
dnaProgramsUserHooks = simpleUserHooks  -- record update coming soon.

-- |We slightly change behaviour of cabal pipeline to accomodate for Halide.
main = defaultMainWithHooks dnaProgramsUserHooks
