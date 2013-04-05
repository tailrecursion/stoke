# stoked

A Leiningen plugin to do many wonderful things.

## Usage

FIXME: Use this for user-level plugins:

Put `[stoked "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your
`:user` profile, or if you are on Leiningen 1.x do `lein plugin install
stoked 0.1.0-SNAPSHOT`.

FIXME: Use this for project-level plugins:

Put `[stoked "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your project.clj.

    user=> (require 'tailrecursion.stoke.edit)
    nil
    user=> (in-ns 'tailrecursion.stoke.edit)
    #<Namespace tailrecursion.stoke.edit>
    user=> (read-file "project.clj")
    ...
    user=> (down)
    ...
    user=> (right 4)
    ...

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
