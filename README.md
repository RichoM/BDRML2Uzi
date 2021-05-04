# BDRML2Uzi

We are currently using [shadow-cljs](https://github.com/thheller/shadow-cljs) for development.

    $ npx shadow-cljs watch dev

Wait until the build is completed. Then open a browser to the url shown in the console (it should be http://localhost:8080).

Connect to the nrepl server as usual. Inside proto-repl evaluate the following:

    $ (shadow.cljs.devtools.api/nrepl-select :dev)

Now you should be able to evaluate code in the context of the browser.
