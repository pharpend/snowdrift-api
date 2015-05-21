# Snowdrift API Guide

At the moment, the Snowdrift "API" is nonexistent, so this is mostly a
filler page until there's actually an API.

If you send the server an HTTP GET request, it will respond with an HTML
version of this page.

If you send the server an HTTP POST request containing JSON data, it
will respond by decoding & re-encoding the JSON data. In theory, this
server, at the moment, is a fully functional JSON minifier.

    $ curl http://localhost:8778 -d '                                                                                                                                                                                          pete@valentine
         {
           "foo": "bar",
           "baz": "bop",
           "pop": {
            "quelle": ["ichthyosis", "rheumatism"],
            "errat": null,
            "lorem": 02113.4885
           }
         }'
    {"pop":{"quelle":["ichthyosis","rheumatism"],"errat":null,"lorem":2113.4885},"foo":"bar","baz":"bop"}
    $
